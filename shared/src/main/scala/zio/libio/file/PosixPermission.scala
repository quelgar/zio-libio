package zio.libio.file

enum FileMode(val mask: Int) {
  case Execute extends FileMode(1)
  case Write extends FileMode(2)
  case Read extends FileMode(4)
  case None extends FileMode(0)
}

opaque type PosixPermission = Int

extension (perm: PosixPermission) {
  def |(other: FileMode): PosixPermission = perm | other.mask

  def isRead: Boolean = (perm & FileMode.Read.mask) != 0
  def isWrite: Boolean = (perm & FileMode.Write.mask) != 0
  def isExecute: Boolean = (perm & FileMode.Execute.mask) != 0

  def asSet: Set[FileMode] =
    Set(FileMode.Read, FileMode.Write, FileMode.Execute)
      .filter(mode => (perm & mode.mask) != 0)
}

object PosixPermission {

  def apply(modes: FileMode*): PosixPermission =
    modes.foldLeft(0)(_ | _.mask)

  val readOnly: PosixPermission = PosixPermission(FileMode.Read)
  val readWrite: PosixPermission =
    PosixPermission(FileMode.Read, FileMode.Write)
  val readExecute: PosixPermission =
    PosixPermission(FileMode.Read, FileMode.Execute)
  val all: PosixPermission =
    PosixPermission(FileMode.Read, FileMode.Write, FileMode.Execute)
  val none: PosixPermission = 0

}

opaque type PosixPermissions = Int

extension (perms: PosixPermissions) {
  def user: PosixPermission = perms >> 6 & 0x7
  def group: PosixPermission = perms >> 3 & 0x7
  def other: PosixPermission = perms & 0x7

  def |(other: PosixPermissions): PosixPermissions = perms | other

  def withUser(user: PosixPermission): PosixPermissions =
    perms | user << 6

  def withGroup(group: PosixPermission): PosixPermissions =
    perms | group << 3

  def withOther(other: PosixPermission): PosixPermissions =
    perms | other
}

object PosixPermissions {

  def apply(
      user: PosixPermission,
      group: PosixPermission,
      other: PosixPermission,
      sticky: Boolean = false,
      setGroupId: Boolean = false,
      setUserId: Boolean = false
  ): PosixPermissions = {
    val stickyMask = if sticky then 0x200 else 0
    val setGroupIdMask = if setGroupId then 0x400 else 0
    val setUserIdMask = if setUserId then 0x800 else 0
    other | (group << 3) | (user << 6) | stickyMask | setGroupIdMask | setUserIdMask
  }

  def user(user: PosixPermission): PosixPermissions =
    PosixPermissions(user, PosixPermission.none, PosixPermission.none)

  def group(group: PosixPermission): PosixPermissions =
    PosixPermissions(PosixPermission.none, group, PosixPermission.none)

  def other(other: PosixPermission): PosixPermissions =
    PosixPermissions(PosixPermission.none, PosixPermission.none, other)

  val userReadWrite: PosixPermissions = user(PosixPermission.readWrite)

}
