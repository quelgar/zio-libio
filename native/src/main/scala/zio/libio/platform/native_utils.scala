package zio
package libio.platform

import scala.scalanative.unsafe.CInt
import scala.scalanative.loop.EventLoop
import zio.libio.IOFailure
import scalauv.LibUv
import scalauv.UvUtils
import scala.scalanative.unsafe.Zone
import scala.scalanative.unsafe.Ptr

val uvLoop = EventLoop.loop

extension (builder: ChunkBuilder[Byte])
  inline def appendNative(nativeBytes: Ptr[Byte], size: Int): builder.type = {
    var pos = 0
    while pos < size do {
      builder += nativeBytes(pos)
      pos += 1
    }
    builder
  }

object uvZIO {

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
        ZIO.succeed(result.toInt)
    }

  def scopedZone[A](f: Zone ?=> A): ZIO[Scope, Throwable, A] = {
    ZIO
      .acquireReleaseWith(ZIO.succeed(Zone.open()))(zone =>
        ZIO.succeed(zone.close())
      )(zone => ZIO.attempt(f(using zone)))
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

}
