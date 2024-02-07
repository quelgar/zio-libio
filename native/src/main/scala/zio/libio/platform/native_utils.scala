package zio
package libio
package platform

import scala.scalanative.loop.EventLoop
import zio.libio.IOFailure
import scala.scalanative.libc
import scalauv.{Buffer as UvBuffer, *}
import scala.scalanative.unsafe
import scala.scalanative.unsigned.*
import zio.libio.IOCtx

private[platform] val uvLoop = EventLoop.loop

private[platform] val UseCurrentOffset = -1L

extension (builder: ChunkBuilder[Byte]) {
  inline private[platform] def appendNative(
      nativeBytes: unsafe.Ptr[Byte],
      size: Int
  ): builder.type = {
    var pos = 0
    while pos < size do {
      builder += nativeBytes(pos)
      pos += 1
    }
    builder
  }

  inline private[platform] def appendNative(
      uvBuffer: UvBuffer
  ): builder.type = {
    appendNative(uvBuffer.base, uvBuffer.length)
  }

  inline private[platform] def appendIOVector(
      max: Int,
      ioVec: IOVector
  ): builder.type = {
    ioVec.foreachBufferMax(max)(appendNative(_))
    builder
  }

}

extension (s: String) {

  inline private[platform] def toScopedCString
      : ZIO[Scope, Nothing, unsafe.CString] = {
    UvZIO.scopedZone(unsafe.toCString(s)).orDie
  }

}

extension (f: file.Path) {

  inline private[platform] def asCString
      : ZIO[Scope, Nothing, unsafe.CString] = {
    f.asString.toScopedCString
  }

}

extension (h: Handle) {

  def loop: Loop = {
    LibUv.uv_handle_get_loop(h)
  }

}

private[platform] object UvZIO {

  def errorNameAndMessage(errorCode: ErrorCode): UIO[String] =
    ZIO.succeed(UvUtils.errorNameAndMessage(errorCode))

  def attemptFsOp[E](
      fsOp: FileReq => unsafe.CInt
  )(handleError: String => E): IO[E, Int] = {
    ZIO.succeed(FileReq.use(fsOp)).handleNativeError
  }

  def attemptFsRead(fsOp: FileReq => unsafe.CInt): IO[IOFailure, Int] = {
    ZIO
      .succeed(FileReq.use(fsOp))
      .handleNativeError
      .filterOrFail(_ > 0)(IOFailure.EndOfFile)
  }

  def attemptFsWrite(
      fsOp: Unsafe ?=> FileReq => unsafe.CInt
  ): IO[IOFailure, Int] =
    attemptFsOp(fsOp)(s => IOFailure.WriteFailed(message = Some(s)))

  def scopedZone[A](
      f: (unsafe.Zone, Unsafe) ?=> A
  ): ZIO[Scope, Throwable, A] = {
    ZIO
      .acquireReleaseWith(ZIO.succeed(unsafe.Zone.open()))(zone =>
        ZIO.succeed(zone.close())
      )(zone => ZIO.attempt(f(using zone)))
  }

  def zoneAllocateBuffer: ZIO[Scope, Nothing, UvBuffer] = {
    for {
      ctx <- NativeContext.current
      bufferSize = ctx.bufferSize
      base <- scopedZone(unsafe.alloc[Byte](bufferSize)).orDie
      uvBuffer <- scopedZone(
        UvBuffer.zoneAllocate(base, bufferSize.toUInt)
      ).orDie
    } yield uvBuffer
  }

  def zoneAllocateIOVector: ZIO[Scope, Nothing, IOVector] = {
    NativeContext.current.flatMap { ctx =>
      scopedZone(IOVector.zoneAllocate(ctx.bufferSize)).orDie
    }
  }

  inline def chunkFromBytes(bytes: unsafe.Ptr[Byte], size: Int)(using
      Unsafe
  ): Chunk[Byte] = {
    val builder = Chunk.newBuilder[Byte]
    var pos = 0
    while pos < size do {
      builder += bytes(pos)
      pos += 1
    }
    builder.result()
  }

  inline def chunkFromBytes(uvBuffer: UvBuffer)(using Unsafe): Chunk[Byte] = {
    chunkFromBytes(uvBuffer.base, uvBuffer.length)
  }

  inline def stackAllocateIOVectorForChunk(
      uvBuffer: UvBuffer,
      chunk: Chunk[Byte]
  )(using Unsafe): (IOVector, Chunk[Byte]) = {
    val length = scala.math.min(uvBuffer.length, chunk.size)
    for i <- 0 until length do {
      uvBuffer.base(i) = chunk(i)
    }
    val remainder = chunk.drop(length)
    (
      IOVector.stackAllocateForBuffer(uvBuffer.base, length.toUInt),
      remainder
    )
  }

}

extension [R](workflow: ZIO[R, Nothing, ErrorCode]) {

  private[platform] def handleNativeError: ZIO[R, IOFailure, ErrorCode] = {
    workflow.flatMap { resultCode =>
      if resultCode < 0 then {
        ZIO.fail(errnoToFailure(resultCode))
      } else {
        ZIO.succeed(resultCode)
      }
    }
  }

}

private[platform] def errnoToFailure(errorCode: ErrorCode): IOFailure = {
  import ErrorCodes.*
  val msg = UvUtils.errorNameAndMessage(errorCode)
  errorCode match {
    case EPERM | EACCES | EROFS =>
      IOFailure.NotPermitted(message = Some(msg))
    case _ =>
      IOFailure.Unknown(message = msg)
  }
}
