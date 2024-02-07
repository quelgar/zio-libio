package zio
package libio
package platform

import stream.*

import scala.scalanative.*
import scalauv.*
import scala.scalanative.unsafe.*
import scala.util.boundary
import scalauv.FileOpenFlags
import scalauv.CreateMode
import scalauv.AccessCheckMode

final class File(fileHandle: FileHandle, loop: Loop)
    extends file.ReadWriteFile {

  def close: UIO[Unit] = ???

  override def read: ZStream[Any, IOFailure, Byte] = {
    ZStream
      .scoped(UvZIO.zoneAllocateIOVector)
      .flatMap { ioVector =>
        ZStream.repeatZIOChunkOption {
          UvZIO
            .attemptFsRead {
              LibUv.uv_fs_read(
                loop,
                _,
                fileHandle,
                ioVector.nativeBuffers,
                ioVector.nativeNumBuffers,
                UseCurrentOffset,
                null
              )
            }
            .mapError {
              case IOFailure.EndOfFile => None
              case other               => Some(other)
            }
            .map { bytesRead =>
              val builder = Chunk.newBuilder[Byte]
              builder.appendIOVector(bytesRead, ioVector)
              builder.result()
            }
        }
      }
  }

  override def readFrom(offset: Long): ZStream[Any, IOFailure, Byte] = {
    ZStream
      .scoped(UvZIO.zoneAllocateIOVector)
      .flatMap { ioVector =>
        ZStream.repeatZIOChunkOption {
          UvZIO
            .attemptFsRead {
              LibUv.uv_fs_read(
                loop,
                _,
                fileHandle,
                ioVector.nativeBuffers,
                ioVector.nativeNumBuffers,
                offset,
                null
              )
            }
            .mapError {
              case IOFailure.EndOfFile => None
              case other               => Some(other)
            }
            .map { bytesRead =>
              val builder = Chunk.newBuilder[Byte]
              builder.appendIOVector(bytesRead, ioVector)
              builder.result()
            }
        }
      }
  }

  override def writeAt(
      offset: Long
  ): ZSink[Any, IOFailure, Byte, Byte, Long] = {
    ZSink.unwrapScoped {
      UvZIO.zoneAllocateBuffer.map { sizedBuf =>
        ZSink.foldLeftChunksZIO(0L) { (writeCount: Long, in: Chunk[Byte]) =>
          ZIO.succeedBlocking {
            boundary {
              var count = 0L
              var bytes = in
              while bytes.nonEmpty do {
                val (ioVec, _) =
                  UvZIO.stackAllocateIOVectorForChunk(sizedBuf, bytes)
                val result = FileReq.use(
                  LibUv.uv_fs_write(
                    loop,
                    _,
                    fileHandle,
                    ioVec.nativeBuffers,
                    ioVec.nativeNumBuffers,
                    offset + writeCount + count,
                    null
                  )
                )
                if result < 0 then {
                  boundary.break(None)
                } else {
                  count += result.toLong
                  bytes = bytes.drop(result)
                }
              }
              Some(count)
            }
          }.handleNativeError
        }
      }
    }
  }

  override def write: ZSink[Any, IOFailure, Byte, Byte, Long] = {
    ZSink.unwrapScoped {
      UvZIO.zoneAllocateBuffer.map { sizedBuf =>
        ZSink.foldLeftChunksZIO(0L) { (writeCount: Long, in: Chunk[Byte]) =>
          ZIO
            .succeedBlocking {
              boundary {
                var count = 0L
                var bytes = in
                while bytes.nonEmpty do {
                  val (ioVec, _) =
                    UvZIO.stackAllocateIOVectorForChunk(sizedBuf, bytes)
                  val result = FileReq
                    .use(
                      LibUv.uv_fs_write(
                        loop,
                        _,
                        fileHandle,
                        ioVec.nativeBuffers,
                        ioVec.nativeNumBuffers,
                        UseCurrentOffset,
                        null
                      )
                    )
                  if result < 0 then {
                    boundary.break(result.toLong)
                  } else {
                    count += result.toLong
                    bytes = bytes.drop(result)
                  }
                }
                count
              }
            }
            .flatMap { result =>
              if result < 0L then
                ZIO.fail(
                  IOFailure.WriteFailed(message =
                    Some(UvUtils.errorNameAndMessage(result.toInt))
                  )
                )
              else ZIO.succeed(result)
            }
        }
      }
    }
  }
}

object FileSpiImplementation extends file.FileSpi {

  private def open(
      path: file.Path,
      flags: Int,
      createMode: Int = CreateMode.None
  ): ZIO[Scope, IOFailure, File] = {
    for {
      filename <- path.asCString
      ctx <- NativeContext.current
      result <- UvZIO.attemptFsOp { openReq =>
        LibUv.uv_fs_open(
          ctx.loop,
          openReq,
          filename,
          flags,
          createMode,
          null
        )
      }(s => IOFailure.OpenFailed(message = Some(s)))
      file = new File(result)
      _ <- ZIO.addFinalizer(file.close)
    } yield file
  }

  override def read(path: file.Path): ZIO[Scope, IOFailure, File] = {
    open(path, FileOpenFlags.O_RDONLY)
  }

  override def write(path: file.Path): ZIO[Scope, IOFailure, File] = {
    open(path, FileOpenFlags.O_WRONLY)
  }

  override def createWrite(
      path: file.Path,
      permissions: file.PosixPermissions
  ): ZIO[Scope, IOFailure, File] = {
    val createMode = ???
    open(
      path,
      FileOpenFlags.O_WRONLY | FileOpenFlags.O_CREAT | FileOpenFlags.O_TRUNC,
      createMode = createMode
    )
  }

  override def readWrite(
      path: file.Path
  ): ZIO[Scope, IOFailure, File] = {
    open(path, FileOpenFlags.O_RDWR)
  }

  override def createReadWrite(
      path: file.Path,
      permissions: file.PosixPermissions
  ): ZIO[Scope, IOFailure, File] = {
    val createMode = ???
    open(
      path,
      FileOpenFlags.O_RDWR | FileOpenFlags.O_CREAT | FileOpenFlags.O_TRUNC,
      createMode = createMode
    )
  }

  override def createTempFile(
      permissions: file.PosixPermissions = file.PosixPermissions.userReadWrite,
      directory: Option[file.Path] = None,
      prefix: String = "",
      suffix: String = ""
  ): ZIO[Scope, IOFailure, file.Path] = ???

  override def exists(path: file.Path): IO[IOFailure, Boolean] =
    ZIO.scoped {
      for {
        filename <- path.asCString
        ctx <- NativeContext.current
        result <- UvZIO.attemptFsOp { req =>
          LibUv.uv_fs_access(
            ctx.loop,
            req,
            filename,
            AccessCheckMode.F_OK,
            null
          )
        }(s => IOFailure.GeneralFailure(message = s))
      } yield result == 0
    }

  override def asAbsolute(path: file.Path): IO[IOFailure, file.Path] =
    ???

}

object FileStats {

  def load(path: file.Path): IO[IOFailure, file.FileStats] = {
    ???
  }
}
