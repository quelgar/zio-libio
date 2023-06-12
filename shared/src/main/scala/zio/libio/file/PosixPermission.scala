package zio.libio.file

enum FileMode(val mask: Int) {
  case Execute extends FileMode(1)
  case Write extends FileMode(2)
  case Read extends FileMode(4)
  case None extends FileMode(0)
}

opaque type PosixPermission = Int

extension (perm: PosixPermission) {
  def +(other: FileMode): PosixPermission =
    perm | other.mask
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

object PosixPermissions {

  def apply(
      user: PosixPermission,
      group: PosixPermission,
      other: PosixPermission,
      sticky: Boolean = false,
      setGroupId: Boolean = false,
      setUserId: Boolean = false
  ): PosixPermissions =
    val stickyMask = if sticky then 0x200 else 0
    val setGroupIdMask = if setGroupId then 0x400 else 0
    val setUserIdMask = if setUserId then 0x800 else 0
    other | (group << 3) | (user << 6) | stickyMask | setGroupIdMask | setUserIdMask

  val userReadWrite: PosixPermissions =
    PosixPermissions(
      PosixPermission.readWrite,
      PosixPermission.none,
      PosixPermission.none
    )

}
