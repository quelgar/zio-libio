package zio
package libio
package platform

import scala.scalanative.unsafe.CInt
import scala.scalanative.loop.EventLoop
import zio.libio.IOFailure
import scalauv.LibUv
import scalauv.UvUtils
import scala.scalanative.libc
import scala.scalanative.unsafe
import unsafe.CString
import scala.scalanative.unsafe.Zone
import scala.scalanative.unsafe.Ptr
import scala.scalanative.unsigned.*
import scalauv.BufferAndSize
import zio.libio.IOCtx
import scalauv.IOVector

private[platform] val uvLoop = EventLoop.loop

private[platform] val UseCurrentOffset = -1L

extension (builder: ChunkBuilder[Byte]) {
  inline private[platform] def appendNative(
      nativeBytes: Ptr[Byte],
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
      bufferandSize: BufferAndSize
  ): builder.type = {
    appendNative(bufferandSize.buffer, bufferandSize.size)
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

  inline private[platform] def toScopedCString: ZIO[Scope, Nothing, CString] = {
    UvZIO.scopedZone(unsafe.toCString(s)).orDie
  }

}

extension (f: file.Path) {

  inline private[platform] def asCString: ZIO[Scope, Nothing, CString] = {
    f.asString.toScopedCString
  }

}

private[platform] object UvZIO {

  def getErrno: UIO[CInt] = ZIO.succeed(libc.errno.errno)

  def errorNameAndMessage: UIO[(CInt, String)] =
    ZIO.succeed(UvUtils.errornoNameAndMessage())

  def attemptFsOp[E](
      fsOp: LibUv.Req => CInt
  )(handleError: String => E): IO[E, Int] = {
    ZIO.succeedBlocking(UvUtils.FsReq.use(fsOp)).handleNativeError
  }

  def attemptFsRead(fsOp: LibUv.Req => CInt): IO[IOFailure, Int] = {
    ZIO
      .succeedBlocking(UvUtils.FsReq.use(fsOp))
      .handleNativeError
      .filterOrFail(_ > 0)(IOFailure.EndOfFile)
  }

  def attemptFsWrite(fsOp: LibUv.Req => CInt): IO[IOFailure, Int] =
    attemptFsOp(fsOp)(s => IOFailure.WriteFailed(message = Some(s)))

  def scopedZone[A](f: Zone ?=> A): ZIO[Scope, Throwable, A] = {
    ZIO
      .acquireReleaseWith(ZIO.succeed(Zone.open()))(zone =>
        ZIO.succeed(zone.close())
      )(zone => ZIO.attempt(f(using zone)))
  }

  def zoneAllocateBuffer: ZIO[Scope & IOCtx, Nothing, BufferAndSize] = {
    ZIO.serviceWith[IOCtx](_.native.bufferSize).flatMap { bufferSize =>
      scopedZone(BufferAndSize.zoneAllocate(bufferSize)).orDie
    }
  }

  def zoneAllocateIOVector: ZIO[Scope & IOCtx, Nothing, IOVector] = {
    ZIO.serviceWith[IOCtx](_.native.bufferSize).flatMap { bufferSize =>
      scopedZone(IOVector.zoneAllocate(bufferSize)).orDie
    }
  }

  inline def chunkFromBytes(bytes: Ptr[Byte], size: Int): Chunk[Byte] = {
    val builder = Chunk.newBuilder[Byte]
    var pos = 0
    while pos < size do {
      builder += bytes(pos)
      pos += 1
    }
    builder.result()
  }

  inline def chunkFromBytes(sizedBuffer: BufferAndSize): Chunk[Byte] = {
    chunkFromBytes(sizedBuffer.buffer, sizedBuffer.size)
  }

  inline def stackAllocateIOVectorForChunk(
      sizedBuffer: BufferAndSize,
      chunk: Chunk[Byte]
  )(using Unsafe): (IOVector, Chunk[Byte]) = {
    val length = scala.math.min(sizedBuffer.size, chunk.size)
    for i <- 0 until length do {
      sizedBuffer.buffer(i) = chunk(i)
    }
    val remainder = chunk.drop(length)
    (
      IOVector.stackAllocateForBuffer(sizedBuffer.buffer, length.toUInt),
      remainder
    )
  }

}

extension [R](workflow: ZIO[R, Nothing, CInt]) {

  private[platform] def handleNativeError: ZIO[R, IOFailure, CInt] = {
    import scalauv.Errno.*
    workflow.flatMap { resultCode =>
      if resultCode < 0 then {
        UvZIO.getErrno.flatMap(errno => ZIO.fail(errnoToFailure(errno)))
      } else {
        ZIO.succeed(resultCode)
      }
    }
  }

}

extension [R, A](workflow: ZIO[R, Nothing, Option[A]]) {

  private[platform] def handleNativeError: ZIO[R, IOFailure, A] = {
    import scalauv.Errno.*
    workflow.someOrElseZIO(
      UvZIO.getErrno.flatMap(errno => ZIO.fail(errnoToFailure(errno)))
    )
  }
}

private[platform] def errnoToFailure(errno: CInt): IOFailure = {
  require(errno < 0)
  import scalauv.Errno.*
  val msg = UvUtils.errorNameAndMessage()
  errno match {
    case EPERM | EACCES | EROFS =>
      IOFailure.NotPermitted(message = Some(msg))
    case _ =>
      IOFailure.Unknown(message = msg)
  }
}
