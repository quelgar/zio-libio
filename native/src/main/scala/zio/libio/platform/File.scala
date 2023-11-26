package zio
package libio
package platform

import stream.*

import scala.scalanative.*
import scalauv.LibUv
import scalauv.UvUtils
import scala.scalanative.unsafe.*
import scalauv.IOVector

final class File(
    fileHandle: LibUv.FileHandle,
    ioVector: IOVector
) extends file.ReadWriteFile {

  // def close: URIO[IOCtx, Unit] = ???

  override def read: ZStream[IOCtx, IOFailure, Byte] = {
    ZStream.repeatZIOChunkOption {
      uvZIO
        .attemptFsRead {
          LibUv.uv_fs_read(
            uvLoop,
            _,
            fileHandle,
            ioVector.nativeBuffers,
            ioVector.nativeNumBuffers,
            -1L,
            null
          )
        }
        .mapError {
          case IOFailure.EndOfFile => None
          case other               => Some(other)
        }
        .map { bytesRead =>
          val builder = Chunk.newBuilder[Byte]
          ioVector.foreachBufferMax(bytesRead) {
            builder.appendNative(_, _)
          }
          builder.result()
        }
    }
  }

  override def readFrom(offset: Long): ZStream[IOCtx, IOFailure, Byte] = ???

  override def writeAt(
      offset: Long
  ): ZSink[IOCtx, IOFailure, Byte, Byte, Long] = ???

  override def write: ZSink[IOCtx, IOFailure, Byte, Byte, Long] = ???

}

object FileSpiImplementation extends file.FileSpi {

  override def read(path: file.Path): ZIO[Scope & IOCtx, IOFailure, File] = ???

  override def write(path: file.Path): ZIO[Scope & IOCtx, IOFailure, File] = ???

  override def createWrite(
      path: file.Path,
      permissions: file.PosixPermissions
  ): ZIO[Scope & IOCtx, IOFailure, File] = ???

  override def readWrite(
      path: file.Path
  ): ZIO[Scope & IOCtx, IOFailure, File] = ???

  override def createReadWrite(
      path: file.Path,
      permissions: file.PosixPermissions
  ): ZIO[Scope & IOCtx, IOFailure, File] = ???

  override def createTempFile(
      permissions: file.PosixPermissions = file.PosixPermissions.userReadWrite,
      directory: Option[file.Path] = None,
      prefix: String = "",
      suffix: String = ""
  ): ZIO[Scope & IOCtx, IOFailure, file.Path] = ???

  override def exists(path: file.Path): ZIO[IOCtx, IOFailure, Boolean] = ???

  override def asAbsolute(path: file.Path): ZIO[IOCtx, IOFailure, file.Path] =
    ???

}

object FileStats {

  def load(path: file.Path): IO[IOFailure, file.FileStats] = {
    ???
  }
}
