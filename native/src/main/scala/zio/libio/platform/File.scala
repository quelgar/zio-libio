package zio
package libio
package platform

import stream.*

import scala.scalanative.*
import unsigned.*
import scalauv.{Buffer => UvBuffer, *}
import scalauv.FileOpenFlags
import scalauv.CreateMode
import scalauv.AccessCheckMode
import java.nio.charset.StandardCharsets
import scala.scalanative.libc.stdlib

final class File(fileHandle: FileHandle, loop: Loop)
    extends file.ReadWriteFile {

  import File.*

  def close: UIO[Unit] = {
    UvZIO.attemptFsOp { req =>
      LibUv.uv_fs_close(loop, req, fileHandle, null)
    }.ignore
  }

  // trait Emit[+R, -E, -A, +B] extends (ZIO[R, Option[E], Chunk[A]] => B) {

  override def read: ZStream[Any, IOFailure, Byte] = {
    readFrom(UseCurrentOffset)
  }

  override def readFrom(offset: Long): ZStream[Any, IOFailure, Byte] = {
    ZStream.asyncScoped[Any, IOFailure, Byte] { emit =>
      for {
        donePromise <- Promise.make[Nothing, Unit]
        uvBuf <- UvZIO.scopedMallocBuffer
        dataPtr <- ScalaNativePointers.makeNativePtr(
          ReadData(emit, uvBuf, fileHandle, donePromise, offset)
        )
        req <- scopedZone(FileReq.zoneAllocate())
        _ <- ZIO
          .succeed {
            req.data = dataPtr
            LibUv.uv_fs_read(
              loop,
              req,
              fileHandle,
              uvBuf,
              1.toUInt,
              UseCurrentOffset,
              callback.onRead
            )
          }
          .handleNativeError
          .withFinalizerExit { (_, exit) =>
            for {
              _ <- ZIO.when(exit.isInterrupted) {
                UvZIO.cancelRequest(req).ignore
              }
              _ <- donePromise.await
              _ <- ZIO.succeed(req.cleanup())
            } yield ()
          }
      } yield ()
    }
  }

  override def writeAt(
      offset: Long
  ): ZSink[Any, IOFailure, Byte, Byte, Long] = {
    ZSink.foldLeftChunksZIO(0L) { (writeCount: Long, in: Chunk[Byte]) =>
      val writePos =
        if offset == UseCurrentOffset then offset else offset + writeCount
      ZIO
        .asyncInterrupt[Any, IOFailure, Long] { zioCallback =>
          val uvBuf = in.mallocUvBuffer()
          val data =
            new WriteData(zioCallback, fileHandle, uvBuf, writePos, 0)
          val req = FileReq.malloc()
          req.data = ScalaNativePointers.unsafe.makeNativePtr(data)
          val result = LibUv.uv_fs_write(
            loop,
            req,
            fileHandle,
            uvBuf,
            1.toUInt,
            writePos,
            callback.onWrite
          )
          if result < 0 then {
            ScalaNativePointers.unsafe.freeObject(data)
            req.free()
            stdlib.free(uvBuf.base)
            uvBuf.free()
            Right(UvZIO.errorCodeToFailure(result))
          } else {
            Left(UvZIO.cancelRequest(req).ignore)
          }
        }
        .map(_ + writeCount)
    }
  }

  override def write: ZSink[Any, IOFailure, Byte, Byte, Long] = {
    writeAt(UseCurrentOffset)
  }

}

object File {

  private final class ReadData(
      val emit: ZIO[Any, Option[IOFailure], Chunk[Byte]] => Any,
      val uvBuffer: UvBuffer,
      val handle: FileHandle,
      val donePromise: Promise[Nothing, Unit],
      var offset: Long
  ) {
    inline def incrementOffset(by: Long): Unit = {
      if offset != UseCurrentOffset then offset += by
    }
  }

  private final class WriteData(
      val callback: ZIO[Any, IOFailure, Long] => Unit,
      val handle: FileHandle,
      val uvBuffer: UvBuffer,
      private val writePos: Long,
      var written: Int
  ) {
    inline def incrementOffset(by: Int): Long = {
      written += by
      if writePos == UseCurrentOffset then writePos else writePos + written
    }
  }

  private object callback {

    val onRead: LibUv.FsCallback = { (req: FileReq) =>
      Unsafe.unsafely {
        val data = ScalaNativePointers.unsafe
          .nativeToScala(req.data)
          .asInstanceOf[ReadData]
        req.result match {
          case ErrorCodes.ECANCELED =>
            data.emit {
              data.donePromise.succeed(()) *> ZIO.interrupt
            }
          case errorCode if errorCode < 0 =>
            data.emit {
              data.donePromise.succeed(()) *>
                UvZIO.errorCodeToFailure(errorCode.toInt).some
            }
          case 0 =>
            data.emit(data.donePromise.succeed(()) *> ZIO.fail(None))
          case bytesRead =>
            data.incrementOffset(bytesRead)
            val chunk =
              data.uvBuffer.withLength(bytesRead.toInt)(UvZIO.chunkFromBytes(_))
            data.emit(ZIO.succeed(chunk))
            LibUv.uv_fs_read(
              req.loop,
              req,
              data.handle,
              data.uvBuffer,
              1.toUInt,
              data.offset,
              onRead
            )
        }
        ()
      }
    }

    val onWrite: LibUv.FsCallback = { (req: FileReq) =>
      Unsafe.unsafely {
        val data = ScalaNativePointers.unsafe
          .nativeToScala(req.data)
          .asInstanceOf[WriteData]
        req.result.toInt match {
          case errorCode if errorCode < 0 =>
            ScalaNativePointers.unsafe.freeObject(data)
            req.cleanup()
            req.free()
            stdlib.free(data.uvBuffer.base)
            data.uvBuffer.free()
            data.callback(UvZIO.errorCodeToFailure(errorCode))
          case bytesWritten
              if data.written + bytesWritten < data.uvBuffer.length =>
            val writePos = data.incrementOffset(bytesWritten)
            val result = data.uvBuffer.withOffset(data.written) {
              LibUv.uv_fs_write(
                req.loop,
                req,
                data.handle,
                _,
                1.toUInt,
                writePos,
                onWrite
              )
            }
            if result < 0 then {
              ScalaNativePointers.unsafe.freeObject(data)
              req.cleanup()
              req.free()
              stdlib.free(data.uvBuffer.base)
              data.uvBuffer.free()
              data.callback(UvZIO.errorCodeToFailure(result))
            }
          case bytesWritten =>
            val totalWritten = data.written + bytesWritten
            ScalaNativePointers.unsafe.freeObject(data)
            req.cleanup()
            req.free()
            stdlib.free(data.uvBuffer.base)
            data.uvBuffer.free()
            data.callback(ZIO.succeed(totalWritten.toLong))
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
      file <- ZIO.acquireRelease {
        UvZIO
          .attemptFsOp { openReq =>
            LibUv.uv_fs_open(
              ctx.loop,
              openReq,
              filename,
              flags,
              createMode,
              null
            )
          }
          .map(new File(_, ctx.loop))
      }(_.close)
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
    val createMode = 0x1ff // TODO fix constants in scalauv
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
    val createMode = 0x1ff // TODO fix constants in scalauv
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
  ): ZIO[Scope, IOFailure, file.Path] = {
    val dirString = directory.map(p => s"$p/").getOrElse("")
    val template = s"${dirString}${prefix}_${suffix}_XXXXXX"
    for {
      ctx <- NativeContext.current
      path <- ZIO.scoped {
        for {
          cTemplate <- template.toScopedCString
          fileInfo <- UvZIO.attemptFsOpWith(
            LibUv.uv_fs_mkstemp(ctx.loop, _, cTemplate, null)
          )(fileReq =>
            (LibUv.uv_fs_get_path(fileReq), LibUv.uv_fs_get_result(fileReq))
          )
          (cPath, handle) = fileInfo
          _ <- UvZIO.attemptFsOp(req =>
            LibUv.uv_fs_close(
              ctx.loop,
              req,
              handle.asInstanceOf[FileHandle],
              null
            )
          )
        } yield unsafe.fromCString(cPath)
      }
      _ <- ZIO.addFinalizer {
        UvZIO.attemptFsOp { req =>
          withZone {
            val cPath = unsafe.toCString(path, StandardCharsets.UTF_8)
            LibUv.uv_fs_unlink(ctx.loop, req, cPath, null)
          }
        }.ignore
      }
    } yield file.Path.fromString(path)
  }

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
        }
      } yield result == 0
    }

  override def asAbsolute(path: file.Path): IO[IOFailure, file.Path] =
    ZIO.scoped {
      for {
        cPath <- path.asCString
        ctx <- NativeContext.current
        cRealPath <- UvZIO.attemptFsOpWith(
          LibUv.uv_fs_realpath(ctx.loop, _, cPath, null)
        )(LibUv.uv_fs_get_ptr)
      } yield file.Path.fromString(
        unsafe.fromCString(cRealPath, StandardCharsets.UTF_8)
      )
    }

}

object FileStats {

  private[platform] def fromNative(
      native: Stat
  )(using Unsafe): file.FileStats = {
    val created = ???
    val lastModified = ???
    val lastAccessed = ???
    val fileType = ???
    val ownerId = native.uid
    val groupId = native.gid
    val size = native.size
    val permissions = ???
    file.FileStats(
      created = created,
      lastModified = lastModified,
      lastAccessed = lastAccessed,
      fileType = fileType,
      ownerId = ownerId.toInt,
      groupId = groupId.toInt,
      size = size.toLong,
      permissions = permissions
    )
  }

  def load(path: file.Path): IO[IOFailure, file.FileStats] = ZIO.scoped {
    for {
      cPath <- path.asCString
      ctx <- NativeContext.current
      stat <- UvZIO.attemptFsOpWith(
        LibUv.uv_fs_stat(ctx.loop, _, cPath, null)
      )(req => fromNative(LibUv.uv_fs_get_statbuf(req)))
    } yield stat
  }

}
