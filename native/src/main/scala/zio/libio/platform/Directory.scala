package zio
package libio
package platform

import stream.*
import file.Path

final class Directory extends file.Directory {

  override def list: Stream[IOFailure, Path] = ???

  override def createDirectory(name: String): IO[IOFailure, Path] = ???

  override def createDirectory(path: Path): IO[IOFailure, Path] = ???

  override def permissions: IO[IOFailure, file.PosixPermissions] = ???
}

object Directory {

  def open(path: file.Path): IO[IOFailure, Directory] = ???

}
