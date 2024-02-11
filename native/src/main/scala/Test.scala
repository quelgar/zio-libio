import scalauv.*
import scala.scalanative.unsafe
import unsafe.*
import scala.scalanative.unsigned.*
import java.nio.charset.StandardCharsets
import zio.*
import zio.libio.*
import zio.stream.*
import zio.libio.file.PosixPermissions

object Test {

  private val loop = LibUv.uv_default_loop()

  private var handle = -1

  private def onRead: LibUv.FsCallback = { (req: FileReq) =>
    val bytesRead = req.result
    val buf = Buffer.unsafeFromPtr(req.data)
    LibUv.uv_fs_req_cleanup(req)
    bytesRead match {
      case errorCode if errorCode < 0 =>
        val errorMsg = UvUtils.errorNameAndMessage(errorCode.toInt)
        println(s"Read failed: $errorMsg")
      case 0 =>
        println("EOF")
      case numRead =>
        println(s"Read $numRead bytes")
        Thread.sleep(1000L)
        LibUv.uv_fs_read(loop, req, handle, buf, 1.toUInt, 0L, onRead)
        ()
    }
  }

  private val numReads = 2

  def main(args: Array[String]): Unit = {

    println(s"args = ${args.toList}")

    withZone {

      val path = "/dev/zero"
      val cPath = unsafe.toCString(path, StandardCharsets.UTF_8)
      handle = FileReq.use { req =>
        LibUv
          .uv_fs_open(loop, req, cPath, FileOpenFlags.O_RDONLY, 0, null)
          .checkErrorThrowIO()
      }
      val iov = IOVector.zoneAllocate(1_000_000)
      val readReq = FileReq.zoneAllocate()
      LibUv.uv_req_set_data(readReq, iov.nativeBuffers.toPtr)
      LibUv
        .uv_fs_read(
          loop,
          readReq,
          handle,
          iov.nativeBuffers,
          iov.nativeNumBuffers,
          0L,
          onRead
        )
        .checkErrorThrowIO()

      for i <- 1 to numReads do {
        println(s"Running $i")
        LibUv.uv_run(loop, RunMode.ONCE)
      }

      println("Cancelling")
      var result = 0
      while { result = readReq.cancel(); result == ErrorCodes.EBUSY } do {
        println(s"Cancel result = ${UvUtils.errorNameAndMessage(result)}")
        LibUv.uv_run(loop, RunMode.ONCE)
        ()
      }
      result.checkErrorThrowIO()
      println("Cancelled")

      LibUv.uv_run(loop, RunMode.DEFAULT)

      val createFile = file.Path.homeDirectory / "tmp" / "create.txt"

      val filename = file.Path.fromString(args.head)
      val program = ZIO
        .scoped {
          for {
            _ <- Console.printLine(s"Reading file: $filename")
            inFile <- file.ReadFile.open(filename)
            stream =
              inFile.read >>> ZPipeline.utf8Decode >>> ZPipeline.splitLines
            _ <- stream.runForeach(Console.printLine(_))

            outFile <- file.WriteFile.create(
              createFile,
              PosixPermissions.userReadWrite
            )
            count <- ZStream("Hello, there world!\n") >>>
              ZPipeline.utf8Encode >>>
              outFile.write
            _ <- Console.printLine(s"Wrote $count bytes to $createFile")
          } yield ()
        }
        .catchAll { e =>
          Console.printLine(s"I/O Error: $e")
        }

      Unsafe.unsafely(Runtime.default.unsafe.run(program).getOrThrow())

    }

    ()
  }
}
