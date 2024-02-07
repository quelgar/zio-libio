package zio
package libio
package platform

import zio.stream.*
import java.nio
import java.nio.{channels as nioc}
import java.io.IOException
import scala.jdk.CollectionConverters.*
import file.*
import java.io.IOError

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
    case e: IOException => IOFailure.IOError(Some(e), e.getMessage())
    case e: IOError     => IOFailure.IOError(Some(e), e.getMessage())
  }

  val mapIOException: IOException => IOFailure = e =>
    IOFailure.IOError(Some(e), e.getMessage())

}

object JavaChannels {

  def streamFrom(
      channel: nioc.ReadableByteChannel,
      buffer: nio.ByteBuffer
  ): Stream[IOFailure, Byte] = {
    ZStream.repeatZIOChunkOption {
      ZIO
        .attemptBlockingIO {
          if channel.read(buffer) < 0 then {
            None
          } else {
            buffer.flip()
            val chunk = JavaBuffers.unsafeCopyToChunk(buffer)
            buffer.clear()
            Some(chunk)
          }
        }
        .mapError(JavaErrors.mapIOException)
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
          ZIO.succeed {
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

  def fromPath(path: file.Path): nio.file.Path = {
    val head = if path.absolute then "/" else ""
    path.stringComponents.nonEmptyOrElse(nio.file.Path.of(head)) { components =>
      nio.file.Path.of(head, components*)
    }
  }

  def toPath(path: nio.file.Path): file.Path = {
    val components =
      Chunk.fromIterator(path.iterator().asScala.map(_.toString()))
    file.Path(path.isAbsolute(), components.map(Path.Component.fromString))
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

object JavaAttribs {

  def fromPermissions(
      permissions: file.PosixPermissions
  ): nio.file.attribute.FileAttribute[java.util.Set[
    nio.file.attribute.PosixFilePermission
  ]] = {
    var javaPerms = Set.empty[nio.file.attribute.PosixFilePermission]
    if permissions.user.isRead then {
      javaPerms += nio.file.attribute.PosixFilePermission.OWNER_READ
    }
    if permissions.user.isWrite then {
      javaPerms += nio.file.attribute.PosixFilePermission.OWNER_WRITE
    }
    if permissions.user.isExecute then {
      javaPerms += nio.file.attribute.PosixFilePermission.OWNER_EXECUTE
    }
    if permissions.group.isRead then {
      javaPerms += nio.file.attribute.PosixFilePermission.GROUP_READ
    }
    if permissions.group.isWrite then {
      javaPerms += nio.file.attribute.PosixFilePermission.GROUP_WRITE
    }
    if permissions.group.isExecute then {
      javaPerms += nio.file.attribute.PosixFilePermission.GROUP_EXECUTE
    }
    if permissions.other.isRead then {
      javaPerms += nio.file.attribute.PosixFilePermission.OTHERS_READ
    }
    if permissions.other.isWrite then {
      javaPerms += nio.file.attribute.PosixFilePermission.OTHERS_WRITE
    }
    if permissions.other.isExecute then {
      javaPerms += nio.file.attribute.PosixFilePermission.OTHERS_EXECUTE
    }
    nio.file.attribute.PosixFilePermissions.asFileAttribute(javaPerms.asJava)
  }
}
