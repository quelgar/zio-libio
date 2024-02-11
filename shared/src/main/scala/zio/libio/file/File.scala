package zio
package libio
package file

import stream.*

trait ReadFile {

  def read: ZStream[Any, IOFailure, Byte]

  def readFrom(offset: Long): ZStream[Any, IOFailure, Byte]

}

object ReadFile {

  def open(path: Path): ZIO[Scope, IOFailure, ReadFile] =
    platform.FileSpiImplementation.read(path)

}

trait WriteFile {

  def writeAt(offset: Long): ZSink[Any, IOFailure, Byte, Byte, Long]

  def write: ZSink[Any, IOFailure, Byte, Byte, Long]

}

object WriteFile {

  def open(path: Path): ZIO[Scope, IOFailure, WriteFile] =
    platform.FileSpiImplementation.write(path)

  def create(
      path: Path,
      permissions: PosixPermissions
  ): ZIO[Scope, IOFailure, WriteFile] =
    platform.FileSpiImplementation.createWrite(path, permissions)

}

trait ReadWriteFile extends ReadFile, WriteFile

object ReadWriteFile {

  def open(path: Path): ZIO[Scope, IOFailure, ReadWriteFile] =
    platform.FileSpiImplementation.readWrite(path)

  def create(
      path: Path,
      permissions: PosixPermissions
  ): ZIO[Scope, IOFailure, ReadWriteFile] =
    platform.FileSpiImplementation.createReadWrite(path, permissions)

}

def createTempFile(
    permissions: PosixPermissions = PosixPermissions.userReadWrite,
    directory: Option[Path] = None,
    prefix: String = "",
    suffix: String = ""
): ZIO[Scope, IOFailure, Path] =
  platform.FileSpiImplementation.createTempFile(
    permissions,
    directory,
    prefix,
    suffix
  )

trait FileSpi {

  def read(path: Path): ZIO[Scope, IOFailure, ReadFile]

  def write(path: Path): ZIO[Scope, IOFailure, WriteFile]

  def createWrite(
      path: Path,
      permissions: PosixPermissions
  ): ZIO[Scope, IOFailure, ReadWriteFile]

  def readWrite(path: Path): ZIO[Scope, IOFailure, ReadWriteFile]

  def createReadWrite(
      path: Path,
      permissions: PosixPermissions
  ): ZIO[Scope, IOFailure, ReadWriteFile]

  def createTempFile(
      permissions: PosixPermissions = PosixPermissions.userReadWrite,
      directory: Option[Path] = None,
      prefix: String = "",
      suffix: String = ""
  ): ZIO[Scope, IOFailure, Path]

  def exists(path: Path): ZIO[Any, IOFailure, Boolean]

  def asAbsolute(path: Path): ZIO[Any, IOFailure, Path]

}
