package zio.libio
package file

import zio.*
import zio.stream.*
import zio.test.*
import java.nio.charset.StandardCharsets

object FileSpec extends ZIOSpecDefault {

  private val testData = Chunk.fromArray(
    "The world is my country, all mankind are my brethren, and to do good is my religion"
      .getBytes(StandardCharsets.UTF_8)
  )

  private val testDataStream = ZStream.fromChunk(testData)

  private val path =
    Path.relative(Up, "shared", "src", "test", "resources", "test.txt")

  extension (spec: Spec[TestEnvironment & Scope & IOCtx, Any])
    def usingIOCtx(ioCtx: IOCtx): Spec[TestEnvironment & Scope, Any] =
      spec
        .transform[TestEnvironment & Scope & IOCtx, Any] {
          case Spec.TestCase(test, annotations) =>
            Spec.TestCase(ioCtx.use(test), annotations)
          case other => other
        }
        .provideSomeEnvironment[TestEnvironment & Scope](_.add(ioCtx))

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("File")(
    test("deletes a temporary file when the scope ends") {
      {
        for {
          result <- ZIO.scoped {
            createTempFile().flatMap(p => p.exists.map((p, _)))
          }
          (path, created) = result
          existsAfterScope <- path.exists
        } yield assertTrue(created && !existsAfterScope)
      }
    },
    test("write to a file") {
      for {
        path <- createTempFile()
        count <- ZIO.scoped {
          WriteFile
            .open(path)
            .flatMap(write => testDataStream.run(write.write))
        }
        readBack <- ZIO.scoped {
          ReadFile.open(path).flatMap(read => read.read.runCollect)
        }
      } yield assertTrue(count == testData.size && readBack == testData)
    },
    test("read an existing file") {
      for {
        data <- ZIO.scoped {
          ReadFile.open(path).flatMap(read => read.read.runCollect)
        }
      } yield assertTrue(data == testData)
    }
  ).usingIOCtx(IOCtx.Default)

}
