package zio
package libio
package file

import zio.test.*
import scala.language.implicitConversions

object PathSpec extends ZIOSpecDefault {

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("PathSpec")(
    suite("relative paths")(
      test("can construct a path relative to parent") {
        val path = Path.relative(Up, "a", "b", "c")
        assertTrue(path.toString == "../a/b/c")
      }
    ),
    suite("absolute paths")(
      test("can convert an absolute path to a string") {
        val path = Path.absolute("a", "b", "c")
        assertTrue(path.toString == "/a/b/c")
      },
      test("can convert to absolute path") {
        for {
          path <- Path.relative(Up, "a", "b", "c").asAbsolute
          expected <- ZIO.succeed(
            java.nio.file.Path.of("..", "a", "b", "c").toAbsolutePath()
          )
        } yield assertTrue(path.toString == expected.toString)
      },
      test("can parse absolute path from string") {
        val path = Path.fromString("/a/b/c")
        val expected = Path.root / "a" / "b" / "c"
        assertTrue(path == expected)
      }
    )
  )
}
