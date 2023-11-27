package zio
package libio.platform

import scala.scalanative.unsafe.CInt
import scala.scalanative.loop.EventLoop
import zio.libio.IOFailure
import scalauv.LibUv
import scalauv.UvUtils
import scala.scalanative.unsafe.Zone
import scala.scalanative.unsafe.Ptr
import scala.scalanative.unsigned.*
import scalauv.BufferAndSize
import zio.libio.IOCtx
import scalauv.IOVector

private[platform] val uvLoop = EventLoop.loop

private[platform] val UseCurrentOffset = -1L

extension (builder: ChunkBuilder[Byte]) {
  inline def appendNative(nativeBytes: Ptr[Byte], size: Int): builder.type = {
    var pos = 0
    while pos < size do {
      builder += nativeBytes(pos)
      pos += 1
    }
    builder
  }

  inline def appendNative(bufferandSize: BufferAndSize): builder.type = {
    appendNative(bufferandSize.buffer, bufferandSize.size)
  }

  inline def appendIOVector(max: Int, ioVec: IOVector): builder.type = {
    ioVec.foreachBufferMax(max)(appendNative(_))
    builder
  }

}

object UvZIO {

  def attemptFsRead(fsOp: LibUv.Req => CInt): IO[IOFailure, Int] =
    ZIO.succeedBlocking(UvUtils.FsReq.use(fsOp)).flatMap {
      case 0 =>
        ZIO.fail(IOFailure.EndOfFile)
      case result if result < 0 =>
        ZIO.fail(
          IOFailure
            .ReadFailed(message = Some(UvUtils.errorNameAndMessage(result)))
        )
      case result =>
        ZIO.succeed(result)
    }

  def attemptFsWrite(fsOp: LibUv.Req => CInt): IO[IOFailure, Int] =
    ZIO.succeedBlocking(UvUtils.FsReq.use(fsOp)).flatMap {
      case result if result < 0 =>
        ZIO.fail(
          IOFailure
            .ReadFailed(message = Some(UvUtils.errorNameAndMessage(result)))
        )
      case result =>
        ZIO.succeed(result)
    }

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
  ): (IOVector, Chunk[Byte]) = {
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
