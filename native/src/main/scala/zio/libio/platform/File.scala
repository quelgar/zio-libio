package zio
package libio
package platform

import stream.*

final class File extends file.ReadWriteFile {

  // def close: URIO[IOCtx, Unit] = ???

  override def read: ZStream[IOCtx, IOFailure, Byte] = {}

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
