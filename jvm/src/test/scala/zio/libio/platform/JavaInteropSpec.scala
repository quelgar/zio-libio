package zio
package libio
package platform

import test.*
import file.given
import java.nio
import nio.file.attribute as attr
import scala.jdk.CollectionConverters.*

object JavaInteropSpec extends ZIOSpecDefault {

  def checkFileAttrib[A](
      actual: attr.FileAttribute[A],
      expected: attr.FileAttribute[A]
  ) =
    assertTrue(
      actual.name() == expected.name() && actual.value() == expected.value()
    )

  override def spec: Spec[TestEnvironment & Scope, Any] =
    suite("JavaInteropSpec")(
      suite("Java paths")(
        test("relative paths can be converted to libio paths") {
          val javaPath = java.nio.file.Paths.get("foo", "bar")
          val path = JavaPath.toPath(javaPath)
          assertTrue(
            path == file.Path.relative("foo", "bar")
          )
        },
        test("absolute paths can be converted to libio paths") {
          val javaPath = java.nio.file.Paths.get("/foo", "bar")
          val path = JavaPath.toPath(javaPath)
          assertTrue(
            path == file.Path.absolute("foo", "bar")
          )
        },
        test("relative paths can be converted from libio paths") {
          val path = file.Path.relative("foo", "bar")
          val javaPath = JavaPath.fromPath(path)
          assertTrue(javaPath == java.nio.file.Paths.get("foo", "bar"))
        },
        test("absolute paths can be converted from libio paths") {
          val path = file.Path.absolute("foo", "bar")
          val javaPath = JavaPath.fromPath(path)
          assertTrue(javaPath == java.nio.file.Paths.get("/foo", "bar"))
        }
      ),
      suite("Permissions")(
        test("can convert to Java file attributes") {
          val permissions = file.PosixPermissions.userReadWrite
          val javaAttrs = JavaAttribs.fromPermissions(permissions)
          val expected = attr.PosixFilePermissions.asFileAttribute(
            Set(
              attr.PosixFilePermission.OWNER_READ,
              attr.PosixFilePermission.OWNER_WRITE
            ).asJava
          )
          checkFileAttrib(javaAttrs, expected)
        }
      )
    )
}
