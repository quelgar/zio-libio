package zio
package libio
package platform

import stream.*
import file.Path
import file.PosixPermissions

final class File extends file.ReadWriteFile {

  override def read(offset: Long = 0L): ZStream[IOCtx, IOFailure, Byte] = ???

  override def write(
      offset: Long = 0L
  ): ZSink[IOCtx, IOFailure, Byte, Byte, Long] = ???

  override def append: ZSink[IOCtx, IOFailure, Byte, Byte, Long] = ???

}

object File {

  def read(path: file.Path): ZIO[IOCtx & Scope, IOFailure, File] = {

    ???
  }

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

  def asAbsolute(path: Path): ZIO[IOCtx, IOFailure, Path] = ???

}

object FileStats {

  def load(path: Path): IO[IOFailure, file.FileStats] = {
    ???
  }
}
