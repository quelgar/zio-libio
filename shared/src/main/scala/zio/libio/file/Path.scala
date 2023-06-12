package zio
package libio
package file

import stream.*

final case class FileStats(
    created: Instant,
    lastModified: Instant,
    lastAccessed: Instant,
    fileType: FileStats.Type,
    ownerId: Int,
    groupId: Int,
    size: Long,
    permissions: PosixPermissions
)

object FileStats {

  enum Type {
    case Directory
    case RegularFile
    case SymbolicLink
    case Other
  }

}

final case class Path(absolute: Boolean, components: Chunk[String]) {

  inline def isRoot: Boolean = absolute && components.isEmpty

  def parent: Option[Path] =
    if (isRoot) None else Some(copy(components = components.dropRight(1)))

  def /(child: String): Path = copy(components = components :+ child)

  def /(child: Path): Path =
    if (absolute) child else copy(components = components ++ child.components)

  def asDirectory: IO[IOFailure, Directory] = platform.Directory.open(this)

  def loadStats: IO[IOFailure, FileStats] = platform.FileStats.load(this)

  def exists: IO[IOFailure, Boolean] = platform.File.exists(this)

}

object Path {

  def relative(first: String, rest: String*): Path =
    Path(absolute = false, first +: Chunk.fromIterable(rest))

  def absolute(first: String, rest: String*): Path =
    Path(absolute = true, first +: Chunk.fromIterable(rest))

  val root: Path = Path(absolute = true, components = Chunk.empty)

  object Root {
    def unapply(path: Path): Boolean = path.isRoot
  }

}

trait Directory {

  def list: Stream[IOFailure, Path]

  def createDirectory(name: String): IO[IOFailure, Path]

  def createDirectory(path: Path): IO[IOFailure, Path]

  def permissions: IO[IOFailure, PosixPermissions]

}
