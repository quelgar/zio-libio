package zio
package libio
package platform

import zio.stream.*
import java.nio
import java.nio.{channels as nioc}
import java.io.IOException

object JavaBuffers {

  def unsafeCopyToChunk(buffer: nio.ByteBuffer)(using Unsafe): Chunk[Byte] = {
    val a = Array.ofDim[Byte](buffer.remaining())
    buffer.get(a)
    Chunk.fromArray(a)
  }

  def unsafeCopyFromChunk(chunk: Chunk[Byte], buffer: nio.ByteBuffer)(using
      Unsafe
  ): Chunk[Byte] = {
    val size = buffer.remaining()
    val (data, remaining) = chunk.splitAt(size)
    data.foreach(buffer.put(_))
    remaining
  }

}

object JavaErrors {

  def refineToIOFailure: PartialFunction[Throwable, IOFailure] = {
    case e: IOException => IOFailure.GeneralFailure(Some(e), e.getMessage())
  }
}

object JavaChannels {

  def streamFrom(
      channel: nioc.ReadableByteChannel,
      buffer: nio.ByteBuffer
  ): Stream[IOFailure, Byte] = {
    ZStream.repeatZIOChunkOption {
      ZIO
        .attemptUnsafe { implicit unsafe =>
          if (channel.read(buffer) < 0) {
            None
          } else {
            buffer.flip()
            val chunk = JavaBuffers.unsafeCopyToChunk(buffer)
            buffer.clear()
            Some(chunk)
          }
        }
        .refineOrDie(JavaErrors.refineToIOFailure)
        .some
    }
  }

  def sinkTo(
      channel: nioc.WritableByteChannel,
      buffer: nio.ByteBuffer
  ): Sink[IOFailure, Byte, Byte, Long] = {
    ZSink
      .foldLeftChunksZIO((0L, Chunk.empty[Byte])) {
        (state: (Long, Chunk[Byte]), chunk: Chunk[Byte]) =>
          val (count, remaining) = state
          ZIO.succeedUnsafe { implicit unsafe =>
            val newRemaining =
              JavaBuffers.unsafeCopyFromChunk(remaining ++ chunk, buffer)
            buffer.flip()
            val bytesWritten = channel.write(buffer)
            (count + bytesWritten, newRemaining)
          }
      }
      .map(_._1)
  }

}

object JavaPath {

  def fromPath(path: file.Path): nio.file.Path =
    path.components.nonEmptyOrElse(nio.file.Path.of("/")) { components =>
      nio.file.Path.of(components.head, components.tail*)
    }
}

final case class JavaInstant(val javaInstant: java.time.Instant)
    extends AnyVal,
      Instant {

  override def seconds: Long = javaInstant.getEpochSecond

  override def nanoSeconds: Long = javaInstant.getNano
}

object JavaInstant {

  def fromInstant(javaInstant: java.time.Instant): Instant =
    JavaInstant(javaInstant)

  def fromFileTime(fileTime: nio.file.attribute.FileTime): Instant =
    fromInstant(fileTime.toInstant())

}
