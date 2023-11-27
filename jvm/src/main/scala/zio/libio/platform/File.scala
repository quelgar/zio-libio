package zio
package libio
package platform

import stream.*
import java.nio
import java.nio.{channels as nioc}
import file.Path
import file.PosixPermissions
import scala.jdk.CollectionConverters.*
import java.io.IOException

final class File(
    val path: Path,
    fileChannel: nioc.FileChannel
) extends file.ReadWriteFile {

  def close: URIO[IOCtx, Unit] = ZIO.succeed(fileChannel.close())

  override def read: ZStream[IOCtx, IOFailure, Byte] =
    ZStream.fromZIO(makeByteBuffer).flatMap { nioBuf =>
      ZStream
        .repeatZIOChunkOption {
          ZIO.attempt {
            if fileChannel.read(nioBuf) < 0 then {
              None
            } else {
              nioBuf.flip()
              val chunk = JavaBuffers.unsafeCopyToChunk(nioBuf)
              nioBuf.clear()
              Some(chunk)
            }
          }.some
        }
        .refineOrDie(JavaErrors.refineToIOFailure)
    }

  override def readFrom(offset: Long): ZStream[IOCtx, IOFailure, Byte] = {
    ZStream
      .fromZIO(makeByteBuffer)
      .flatMap { nioBuf =>
        ZStream.repeatZIOChunkOption {
          ZIO.attemptBlockingCancelable {
            val bytesRead = fileChannel.read(nioBuf, offset)
            if bytesRead == -1 then None
            else {
              nioBuf.flip()
              val bytes = Array.ofDim[Byte](nioBuf.remaining())
              nioBuf.get(bytes)
              nioBuf.clear()
              Some(Chunk.fromArray(bytes))
            }
          }(close).some
        }
      }
      .refineOrDie(JavaErrors.refineToIOFailure)
  }

  override def write: ZSink[IOCtx, IOFailure, Byte, Byte, Long] = {
    ZSink
      .fromZIO(makeByteBuffer)
      .flatMap { nioBuf =>
        ZSink.foldLeftChunksZIO(0L) { (count, chunk: Chunk[Byte]) =>
          ZIO.attemptBlockingCancelable {
            nioBuf.put(chunk.toArray)
            nioBuf.flip()
            var c = count
            while nioBuf.hasRemaining() do {
              c += fileChannel.write(nioBuf).toLong
            }
            nioBuf.clear()
            c
          }(close)
        }
      }
      .refineOrDie(JavaErrors.refineToIOFailure)
  }

  override def writeAt(
      offset: Long
  ): ZSink[IOCtx, IOFailure, Byte, Byte, Long] = {
    ZSink
      .fromZIO(makeByteBuffer)
      .flatMap { nioBuf =>
        ZSink.foldLeftChunksZIO(0L) { (count, chunk: Chunk[Byte]) =>
          ZIO.attemptBlockingCancelable {
            nioBuf.put(chunk.toArray)
            nioBuf.flip()
            var c = 0L
            while nioBuf.hasRemaining() do {
              c += fileChannel.write(nioBuf, offset + c).toLong
            }
            nioBuf.clear()
            count + c
          }(close)
        }
      }
      .refineOrDie(JavaErrors.refineToIOFailure)
  }

}

object FileSpiImplementation extends file.FileSpi {

  private def open(
      path: file.Path,
      options: Set[nio.file.OpenOption]
  ): ZIO[Scope & IOCtx, IOFailure, File] = {
    ZIO
      .fromAutoCloseable {
        ZIO
          .attempt {
            val javaPath = JavaPath.fromPath(path)
            nioc.FileChannel.open(javaPath, options.asJava)
          }
          .refineOrDie(JavaErrors.refineToIOFailure)
      }
      .map(new File(path, _))
  }

  override def read(path: file.Path): ZIO[Scope & IOCtx, IOFailure, File] = {
    open(path, Set(nio.file.StandardOpenOption.READ))
  }

  override def write(path: file.Path): ZIO[Scope & IOCtx, IOFailure, File] = {
    open(path, Set(nio.file.StandardOpenOption.WRITE))
  }

  override def createWrite(
      path: file.Path,
      permissions: PosixPermissions
  ): ZIO[Scope & IOCtx, IOFailure, File] = {
    open(
      path,
      Set(
        nio.file.StandardOpenOption.CREATE,
        nio.file.StandardOpenOption.WRITE,
        nio.file.StandardOpenOption.TRUNCATE_EXISTING
      )
    )
  }

  override def readWrite(path: file.Path): ZIO[Scope & IOCtx, IOFailure, File] =
    open(
      path,
      Set(
        nio.file.StandardOpenOption.READ,
        nio.file.StandardOpenOption.WRITE
      )
    )

  override def createReadWrite(
      path: file.Path,
      permissions: PosixPermissions
  ): ZIO[Scope & IOCtx, IOFailure, File] = open(
    path,
    Set(
      nio.file.StandardOpenOption.CREATE,
      nio.file.StandardOpenOption.READ,
      nio.file.StandardOpenOption.WRITE,
      nio.file.StandardOpenOption.TRUNCATE_EXISTING
    )
  )

  override def createTempFile(
      permissions: PosixPermissions = PosixPermissions.userReadWrite,
      directory: Option[Path] = None,
      prefix: String = "",
      suffix: String = ""
  ): ZIO[Scope & IOCtx, IOFailure, Path] = {
    ZIO
      .attempt {
        val attr = JavaAttribs.fromPermissions(permissions)
        directory
          .map { d =>
            nio.file.Files.createTempFile(
              JavaPath.fromPath(d),
              prefix,
              suffix,
              attr
            )
          }
          .getOrElse(nio.file.Files.createTempFile(prefix, suffix, attr))
      }
      .map(JavaPath.toPath)
      .refineOrDie(JavaErrors.refineToIOFailure)
      .withFinalizer { path =>
        ZIO
          .attempt(nio.file.Files.deleteIfExists(JavaPath.fromPath(path)))
          .ignore
      }
  }

  override def exists(path: Path): ZIO[IOCtx, IOFailure, Boolean] =
    ZIO
      .attempt(nio.file.Files.exists(JavaPath.fromPath(path)))
      .refineOrDie(JavaErrors.refineToIOFailure)

  override def asAbsolute(path: Path): ZIO[IOCtx, IOFailure, Path] =
    ZIO
      .attempt {
        val javaPath = JavaPath.fromPath(path)
        JavaPath.toPath(javaPath.toAbsolutePath())
      }
      .refineOrDie(JavaErrors.refineToIOFailure)

}

object FileStats {

  def load(path: Path): IO[IOFailure, file.FileStats] = {
    import nio.file.attribute.{PosixFilePermission => JavaPerm}
    ZIO.succeed {
      val javaPath = JavaPath.fromPath(path)
      val views = javaPath.getFileSystem().supportedFileAttributeViews()
      val unixMap = if views.contains("unix") then {
        nio.file.Files.readAttributes(javaPath, "unix:*").asScala
      } else {
        Map.empty[String, Object]
      }
      val basicAttrs = nio.file.Files.readAttributes(
        javaPath,
        classOf[nio.file.attribute.BasicFileAttributes]
      )
      val fileType =
        if basicAttrs.isDirectory() then file.FileStats.Type.Directory
        else if basicAttrs.isRegularFile() then file.FileStats.Type.RegularFile
        else if basicAttrs.isSymbolicLink() then
          file.FileStats.Type.SymbolicLink
        else file.FileStats.Type.Other

      val javaPerms = unixMap
        .get("permissions")
        .map(_.asInstanceOf[java.util.Set[Object]].asScala)
        .getOrElse(Set.empty[Object])
      val permissions = PosixPermissions(
        user = file.PosixPermission(
          if javaPerms(JavaPerm.OWNER_READ) then file.FileMode.Read
          else file.FileMode.None,
          if javaPerms(JavaPerm.OWNER_WRITE) then file.FileMode.Write
          else file.FileMode.None,
          if javaPerms(JavaPerm.OWNER_EXECUTE) then file.FileMode.Execute
          else file.FileMode.None
        ),
        group = file.PosixPermission(
          if javaPerms(JavaPerm.GROUP_READ) then file.FileMode.Read
          else file.FileMode.None,
          if javaPerms(JavaPerm.GROUP_WRITE) then file.FileMode.Write
          else file.FileMode.None,
          if javaPerms(JavaPerm.GROUP_EXECUTE) then file.FileMode.Execute
          else file.FileMode.None
        ),
        other = file.PosixPermission(
          if javaPerms(JavaPerm.OTHERS_READ) then file.FileMode.Read
          else file.FileMode.None,
          if javaPerms(JavaPerm.OTHERS_WRITE) then file.FileMode.Write
          else file.FileMode.None,
          if javaPerms(JavaPerm.OTHERS_EXECUTE) then file.FileMode.Execute
          else file.FileMode.None
        )
      )

      file.FileStats(
        created = JavaInstant.fromFileTime(basicAttrs.creationTime()),
        lastModified = JavaInstant.fromFileTime(basicAttrs.lastModifiedTime()),
        lastAccessed = JavaInstant.fromFileTime(basicAttrs.lastAccessTime()),
        fileType = fileType,
        ownerId = unixMap.getOrElse("uid", 0).asInstanceOf[Int],
        groupId = unixMap.getOrElse("gid", 0).asInstanceOf[Int],
        size = basicAttrs.size(),
        permissions = permissions
      )
    }

  }
}
