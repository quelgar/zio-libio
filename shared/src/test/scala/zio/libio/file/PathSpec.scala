package zio
package libio
package file

import zio.test.*

object PathSpec extends ZIOSpecDefault {

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("Path")(
    suite("relative paths")(
      test("can construct a path relative to parent") {
        val path = Path.relative(Up, "a", "b", "c")
        assertTrue(path.asString == "../a/b/c")
      }
    ),
    suite("absolute paths")(
      test("can convert an absolute path to a string") {
        val path = Path.absolute("a", "b", "c")
        assertTrue(path.asString == "/a/b/c")
      },
      test("can convert to absolute path") {
        for {
          path <- Path.relative(Up, "a", "b", "c").asAbsolute
          expected <- ZIO.succeed(
            java.nio.file.Path.of("..", "a", "b", "c").toAbsolutePath()
          )
        } yield assertTrue(path.asString == expected.toString)
      }
    )
  ).usingIOCtx()
}
