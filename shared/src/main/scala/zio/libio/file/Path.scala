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

final case class Path(absolute: Boolean, components: Chunk[Path.Component]) {

  inline def isRoot: Boolean = absolute && components.isEmpty

  def parent: Option[Path] =
    NonEmptyChunk
      .fromChunk(components)
      .map(cs => copy(components = cs.dropRight(1)))

  def /(child: String): Path = copy(components = components :+ child.p)

  def /(child: Path): Path =
    if absolute then child
    else copy(components = components ++ child.components)

  def asDirectory: IO[IOFailure, Directory] =
    platform.Directory.open(this)

  def loadStats: IO[IOFailure, FileStats] =
    platform.FileStats.load(this)

  def exists: IO[IOFailure, Boolean] =
    platform.FileSpiImplementation.exists(this)

  def asAbsolute: IO[IOFailure, Path] =
    if absolute then ZIO.succeed(this)
    else platform.FileSpiImplementation.asAbsolute(this)

  def stringComponents: Chunk[String] = components.map(_.asString)

  override def toString: String = {
    val initial = if absolute then "/" else ""
    stringComponents.mkString(initial, java.io.File.separator, "")
  }

}

object Path {

  enum Component {
    case Up
    case Down(name: String)

    def asString: String = this match {
      case Component.Up => ".."
      case Down(name)   => name
    }
  }

  object Component {
    def fromString(s: String): Component = s match {
      case ".." => Up
      case name => Down(name)
    }
  }

  def currentDirectory: Path = Path(absolute = false, Chunk.empty)

  def currentDirectoryAbsolute: IO[IOFailure, Path] =
    currentDirectory.asAbsolute

  def relative(first: Path.Component, rest: Path.Component*): Path =
    Path(absolute = false, first +: Chunk.fromIterable(rest))

  def absolute(first: Path.Component, rest: Path.Component*): Path =
    Path(absolute = true, first +: Chunk.fromIterable(rest))

  private val unixPathRegex = """/+""".r
  private val windowsPathRegex = """\+""".r

  def fromString(s: String): Path = {
    val splitter =
      if java.io.File.separator == "\\" then windowsPathRegex else unixPathRegex
    val components = Chunk.fromArray(splitter.split(s)).map(_.p)
    val isAbsolute = s.startsWith(java.io.File.separator)
    val trimmedComponents =
      if isAbsolute then components.drop(1) else components
    Path(isAbsolute, trimmedComponents)
  }

  val root: Path = Path(absolute = true, components = Chunk.empty)

  val homeDirectory: Path = Path.fromString(sys.props("user.home"))

  object Root {
    def unapply(path: Path): Boolean = path.isRoot
  }

}

val Up: Path.Component = Path.Component.Up

given Conversion[String, Path.Component] = Path.Component.Down(_)

extension (s: String) {

  inline def p: Path.Component = Path.Component.Down(s)

}

trait Directory {

  def list: Stream[IOFailure, Path]

  def createDirectory(name: String): IO[IOFailure, Path]

  def createDirectory(path: Path): IO[IOFailure, Path]

  def permissions: IO[IOFailure, PosixPermissions]

}
