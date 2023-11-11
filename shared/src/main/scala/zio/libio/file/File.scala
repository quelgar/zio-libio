package zio
package libio
package file

import stream.*
import java.nio

trait ReadFile {

  def read: ZStream[IOCtx, IOFailure, Byte]

  def readFrom(offset: Long): ZStream[IOCtx, IOFailure, Byte]

}

object ReadFile {

  def open(path: Path): ZIO[Scope & IOCtx, IOFailure, ReadFile] =
    platform.FileSpiImplementation.read(path)

}

trait WriteFile {

  def writeAt(offset: Long): ZSink[IOCtx, IOFailure, Byte, Byte, Long]

  def write: ZSink[IOCtx, IOFailure, Byte, Byte, Long]

}

object WriteFile {

  def open(path: Path): ZIO[Scope & IOCtx, IOFailure, WriteFile] =
    platform.FileSpiImplementation.write(path)

  def create(
      path: Path,
      permissions: PosixPermissions
  ): ZIO[Scope & IOCtx, IOFailure, WriteFile] =
    platform.FileSpiImplementation.createWrite(path, permissions)

}

trait ReadWriteFile extends ReadFile, WriteFile

object ReadWriteFile {

  def open(path: Path): ZIO[Scope & IOCtx, IOFailure, ReadWriteFile] =
    platform.FileSpiImplementation.readWrite(path)

  def create(
      path: Path,
      permissions: PosixPermissions
  ): ZIO[Scope & IOCtx, IOFailure, ReadWriteFile] =
    platform.FileSpiImplementation.createReadWrite(path, permissions)

}

def createTempFile(
    permissions: PosixPermissions = PosixPermissions.userReadWrite,
    directory: Option[Path] = None,
    prefix: String = "",
    suffix: String = ""
): ZIO[Scope & IOCtx, IOFailure, Path] =
  platform.FileSpiImplementation.createTempFile(
    permissions,
    directory,
    prefix,
    suffix
  )

trait FileSpi {

  def read(path: Path): ZIO[Scope & IOCtx, IOFailure, ReadFile]

  def write(path: Path): ZIO[Scope & IOCtx, IOFailure, WriteFile]

  def createWrite(
      path: Path,
      permissions: PosixPermissions
  ): ZIO[Scope & IOCtx, IOFailure, ReadWriteFile]

  def readWrite(path: Path): ZIO[Scope & IOCtx, IOFailure, ReadWriteFile]

  def createReadWrite(
      path: Path,
      permissions: PosixPermissions
  ): ZIO[Scope & IOCtx, IOFailure, ReadWriteFile]

  def createTempFile(
      permissions: PosixPermissions = PosixPermissions.userReadWrite,
      directory: Option[Path] = None,
      prefix: String = "",
      suffix: String = ""
  ): ZIO[Scope & IOCtx, IOFailure, Path]

  def exists(path: Path): ZIO[IOCtx, IOFailure, Boolean]

  def asAbsolute(path: Path): ZIO[IOCtx, IOFailure, Path]

}
