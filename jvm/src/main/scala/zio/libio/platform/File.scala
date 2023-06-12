package zio
package libio
package platform

import stream.*
import java.nio
import java.nio.{channels as nioc}
import file.Path
import file.PosixPermissions
import scala.jdk.CollectionConverters.*

final class File(
    val path: Path,
    fileChannel: nioc.FileChannel,
    nioBuf: nio.ByteBuffer
) extends file.ReadWriteFile {

  override def read(offset: Long = 0L): Stream[IOFailure, Byte] = {
    ZStream.fromZIO(ZIO.attemptBlockingIO(fileChannel.position(offset))) *>
      ZStream.repeatZIOChunkOption {
        ZIO.attemptBlocking {
          val bytesRead = fileChannel.read(nioBuf)
          if (bytesRead == -1) None
          else {
            nioBuf.flip()
            val bytes = Array.ofDim[Byte](nioBuf.remaining())
            nioBuf.get(bytes)
            nioBuf.clear()
            Some(Chunk.fromArray(bytes))
          }
        }.
      }
  }

  override def append: Sink[IOFailure, Byte, Byte, Long] = ???

  override def write(offset: Long = 0L): Sink[IOFailure, Byte, Byte, Long] = ???
}

object File {

  def read(path: file.Path): ZIO[Scope, IOFailure, File] = ???

  def write(path: file.Path): ZIO[Scope, IOFailure, File] = ???

  def createWrite(
      path: file.Path,
      permissions: PosixPermissions
  ): ZIO[Scope, IOFailure, File] = ???

  def createTemp(
      permissions: PosixPermissions = PosixPermissions.userReadWrite,
      directory: Option[Path] = None,
      prefix: String = "",
      suffix: String = ""
  ): ZIO[Scope, IOFailure, Path] = ???

  def readWrite(path: file.Path): ZIO[Scope, IOFailure, File] = ???

  def createReadWrite(
      path: file.Path,
      permissions: PosixPermissions
  ): ZIO[Scope, IOFailure, File] = ???

  def exists(path: Path): IO[IOFailure, Boolean] = ???

}

object FileStats {

  def load(path: Path): IO[IOFailure, file.FileStats] = {
    import nio.file.attribute.{PosixFilePermission => JavaPerm}
    ZIO.succeed {
      val javaPath = JavaPath.fromPath(path)
      val views = javaPath.getFileSystem().supportedFileAttributeViews()
      val unixMap = if (views.contains("unix")) {
        nio.file.Files.readAttributes(javaPath, "unix:*").asScala
      } else {
        Map.empty[String, Object]
      }
      val basicAttrs = nio.file.Files.readAttributes(
        javaPath,
        classOf[nio.file.attribute.BasicFileAttributes]
      )
      val fileType =
        if (basicAttrs.isDirectory())
          file.FileStats.Type.Directory
        else if (basicAttrs.isRegularFile())
          file.FileStats.Type.RegularFile
        else if (basicAttrs.isSymbolicLink())
          file.FileStats.Type.SymbolicLink
        else
          file.FileStats.Type.Other

      val javaPerms = unixMap
        .get("permissions")
        .map(_.asInstanceOf[java.util.Set[Object]].asScala)
        .getOrElse(Set.empty[Object])
      val permissions = PosixPermissions(
        user = file.PosixPermission(
          if (javaPerms(JavaPerm.OWNER_READ)) file.FileMode.Read
          else file.FileMode.None,
          if (javaPerms(JavaPerm.OWNER_WRITE)) file.FileMode.Write
          else file.FileMode.None,
          if (javaPerms(JavaPerm.OWNER_EXECUTE)) file.FileMode.Execute
          else file.FileMode.None
        ),
        group = file.PosixPermission(
          if (javaPerms(JavaPerm.GROUP_READ)) file.FileMode.Read
          else file.FileMode.None,
          if (javaPerms(JavaPerm.GROUP_WRITE)) file.FileMode.Write
          else file.FileMode.None,
          if (javaPerms(JavaPerm.GROUP_EXECUTE)) file.FileMode.Execute
          else file.FileMode.None
        ),
        other = file.PosixPermission(
          if (javaPerms(JavaPerm.OTHERS_READ)) file.FileMode.Read
          else file.FileMode.None,
          if (javaPerms(JavaPerm.OTHERS_WRITE)) file.FileMode.Write
          else file.FileMode.None,
          if (javaPerms(JavaPerm.OTHERS_EXECUTE)) file.FileMode.Execute
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
