package zio
package libio
package file

import stream.*

trait ReadFile {

  def read(offset: Long = 0L): Stream[IOFailure, Byte]

}

object ReadFile {

  def open(path: Path): ZIO[Scope, IOFailure, ReadFile] =
    platform.File.read(path)

}

trait WriteFile {

  def write(offset: Long = 0L): Sink[IOFailure, Byte, Byte, Long]

  def append: Sink[IOFailure, Byte, Byte, Long]

}

object WriteFile {

  def open(path: Path): ZIO[Scope, IOFailure, WriteFile] =
    platform.File.write(path)

  def create(
      path: Path,
      permissions: PosixPermissions
  ): ZIO[Scope, IOFailure, WriteFile] =
    platform.File.createWrite(path, permissions)

}

trait ReadWriteFile extends ReadFile, WriteFile

object ReadWriteFile {

  def open(path: Path): ZIO[Scope, IOFailure, ReadWriteFile] =
    platform.File.readWrite(path)

  def create(
      path: Path,
      permissions: PosixPermissions
  ): ZIO[Scope, IOFailure, ReadWriteFile] =
    platform.File.createReadWrite(path, permissions)

}

def createTempFile(
    permissions: PosixPermissions = PosixPermissions.userReadWrite,
    directory: Option[Path] = None,
    prefix: String = "",
    suffix: String = ""
): ZIO[Scope, IOFailure, Path] =
  platform.File.createTemp(permissions, directory, prefix, suffix)
