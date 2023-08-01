package zio
package libio
package file

import test.*

object PosixPermissionSpec extends ZIOSpecDefault {

  override def spec: Spec[TestEnvironment & Scope, Any] =
    suite("PosixPermissionSpec")(
      test("extracts user permissions") {
        val perms = PosixPermissions.userReadWrite
        assertTrue(perms.user.asSet == Set(FileMode.Read, FileMode.Write))
      },
      test("extracts group permissions") {
        val perms = PosixPermissions.group(PosixPermission.readExecute)
        assertTrue(perms.group.asSet == Set(FileMode.Read, FileMode.Execute))
      },
      test("extracts other permissions") {
        val perms = PosixPermissions.other(PosixPermission.readWrite)
        assertTrue(perms.other.asSet == Set(FileMode.Read, FileMode.Write))
      }
    )
}
