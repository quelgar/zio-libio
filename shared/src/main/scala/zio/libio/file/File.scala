package zio
package libio
package file

import stream.*

trait ReadFile {

  def read: ZStream[IOCtx, IOFailure, Byte]

  def readFrom(offset: Long): ZStream[IOCtx, IOFailure, Byte]

}

object ReadFile {

  def open(path: Path): ZIO[Scope & IOCtx, IOFailure, ReadFile] =
    platform.File.read(path)

}

trait WriteFile {

  def writeAt(offset: Long): ZSink[IOCtx, IOFailure, Byte, Byte, Long]

  def write: ZSink[IOCtx, IOFailure, Byte, Byte, Long]

}

object WriteFile {

  def open(path: Path): ZIO[Scope & IOCtx, IOFailure, WriteFile] =
    platform.File.write(path)

  def create(
      path: Path,
      permissions: PosixPermissions
  ): ZIO[Scope & IOCtx, IOFailure, WriteFile] =
    platform.File.createWrite(path, permissions)

}

trait ReadWriteFile extends ReadFile, WriteFile

object ReadWriteFile {

  def open(path: Path): ZIO[Scope & IOCtx, IOFailure, ReadWriteFile] =
    platform.File.readWrite(path)

  def create(
      path: Path,
      permissions: PosixPermissions
  ): ZIO[Scope & IOCtx, IOFailure, ReadWriteFile] =
    platform.File.createReadWrite(path, permissions)

}

def createTempFile(
    permissions: PosixPermissions = PosixPermissions.userReadWrite,
    directory: Option[Path] = None,
    prefix: String = "",
    suffix: String = ""
): ZIO[Scope & IOCtx, IOFailure, Path] =
  platform.File.createTemp(permissions, directory, prefix, suffix)
