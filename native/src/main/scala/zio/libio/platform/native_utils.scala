package zio
package libio
package platform

import scala.scalanative.libc
import scalauv.{Buffer as UvBuffer, *}
import scala.scalanative.unsafe
import scala.scalanative.unsigned.*
import java.io.IOException
import scala.scalanative.libc.stdlib

private[platform] val UseCurrentOffset = -1L

private[platform] def scopedZone[A](
    f: (unsafe.Zone, Unsafe) ?=> A
): ZIO[Scope, Nothing, A] = {
  ZIO
    .acquireReleaseWith(ZIO.succeed(unsafe.Zone.open()))(zone =>
      ZIO.succeed(zone.close())
    )(zone => ZIO.succeed(f(using zone)))
}

private[platform] def scopedMalloc(
    size: Int
): ZIO[Scope, Nothing, unsafe.Ptr[Byte]] = {
  ZIO
    .succeed(stdlib.malloc(size.toULong))
    .withFinalizer(ptr => ZIO.succeed(stdlib.free(ptr)))
}

extension (builder: ChunkBuilder[Byte]) {
  inline private[platform] def appendNative(buffer: UvBuffer): builder.type = {
    buffer.foreach(builder += _)
    builder
  }

  inline private[platform] def appendIOVector(
      max: Int,
      ioVec: IOVector
  ): builder.type = {
    ioVec.foreachBufferMax(max) { uvBuffer =>
      appendNative(uvBuffer)
      ()
    }
    builder
  }

}

extension (chunk: Chunk[Byte]) {

  /** Copies the bytes from this chunk into the given Libuv buffer. If the Libuv
    * buffer is smaller than this chunk, then only the first `uvBuffer.length`
    * bytes are copied. The length of the Libuv buffer is set to the number of
    * bytes copied.
    *
    * @param uvBuffer
    *   The Libuv buffer to copy the bytes into.
    * @return
    *   The remaining bytes in this chunk that did not fit into the Libuv
    *   buffer, empty if the Libuv buffer was large enough to hold all the
    *   bytes.
    */
  private[platform] def copyToUvBuffer(
      uvBuffer: UvBuffer
  )(using Unsafe): Chunk[Byte] = {
    val base = uvBuffer.base
    val length = uvBuffer.length
    val count = math.min(length, chunk.length)
    var pos = 0
    while pos < count do {
      base(pos) = chunk(pos)
      pos += 1
    }
    uvBuffer.length = count
    chunk.drop(count)
  }

  private[platform] inline def mallocUvBuffer()(using Unsafe): UvBuffer = {
    val length = chunk.size.toULong
    val base = stdlib.malloc(length)
    val uvBuffer = UvBuffer.malloc(base, length)
    chunk.copyToUvBuffer(uvBuffer)
    uvBuffer
  }

}

extension (s: String) {

  inline private[platform] def toScopedCString
      : ZIO[Scope, Nothing, unsafe.CString] = {
    scopedZone(unsafe.toCString(s))
  }

}

extension (f: file.Path) {

  inline private[platform] def asCString
      : ZIO[Scope, Nothing, unsafe.CString] = {
    f.toString.toScopedCString
  }

}

extension [R](workflow: ZIO[R, Nothing, ErrorCode]) {

  private[platform] def handleNativeError: ZIO[R, IOFailure, ErrorCode] = {
    workflow.flatMap { resultCode =>
      if resultCode < 0 then {
        UvZIO.errorCodeToFailure(resultCode)
      } else {
        ZIO.succeed(resultCode)
      }
    }
  }

}

extension (zioObject: ZIO.type) {

  def suspendSucceedUnsafely[R, E, A](
      effect: Unsafe ?=> ZIO[R, E, A]
  ): ZIO[R, E, A] = {
    zioObject.suspendSucceedUnsafe(effect)
  }

}

private[platform] object UvZIO {

  def errorNameAndMessage(errorCode: ErrorCode): UIO[String] =
    ZIO.succeed(UvUtils.errorNameAndMessage(errorCode))

  def attemptFsOp(
      fsOp: Unsafe ?=> FileReq => unsafe.CInt
  ): IO[IOFailure, unsafe.CInt] = {
    ZIO.succeed(FileReq.use(fsOp)).handleNativeError
  }

  /** Performs a file system operation and extracts results from the request.
    * This lets you avoid needing to manually manage the request lifecycle.
    *
    * @param fsOp
    *   The file system operation to perform.
    * @param useReq
    *   A function that extracts fields from the request after the operation
    *   completes successfully.
    */
  def attemptFsOpWith[A](
      fsOp: Unsafe ?=> FileReq => unsafe.CInt
  )(useReq: Unsafe ?=> FileReq => A): IO[IOFailure, A] = ZIO.scoped {
    for {
      req <- scopedZone(FileReq.zoneAllocate())
      _ <- ZIO
        .succeed(fsOp(req))
        .ensuring(ZIO.succeed(LibUv.uv_fs_req_cleanup(req)))
        .handleNativeError
      result <- ZIO.succeed(useReq(req))
    } yield result
  }

  def attemptFsRead(fsOp: FileReq => unsafe.CInt): IO[IOFailure, Int] = {
    ZIO
      .succeed(FileReq.use(fsOp))
      .handleNativeError
      .filterOrFail(_ > 0)(IOFailure.EndOfFile)
  }

  def mallocBuffer: UIO[UvBuffer] = {
    for {
      ctx <- NativeContext.current
      bufferSize = ctx.bufferSize
      uvBuf <- ZIO.succeed {
        val base = stdlib.malloc(bufferSize.toULong)
        UvBuffer.malloc(base, bufferSize.toUInt)
      }
    } yield uvBuf
  }

  def scopedMallocBuffer: ZIO[Scope, Nothing, UvBuffer] = {
    for {
      ctx <- NativeContext.current
      bufferSize = ctx.bufferSize
      base <- scopedMalloc(bufferSize)
      uvBuf <- ZIO
        .succeed(UvBuffer.malloc(base, bufferSize.toUInt))
        .withFinalizer(buf => ZIO.succeed(buf.free()))
    } yield uvBuf
  }

  inline def chunkFromBytes(bytes: unsafe.Ptr[Byte], size: Int)(using
      Unsafe
  ): Chunk[Byte] = {
    val builder = Chunk.newBuilder[Byte]
    var pos = 0
    while pos < size do {
      builder += bytes(pos)
      pos += 1
    }
    builder.result()
  }

  inline def chunkFromBytes(uvBuffer: UvBuffer)(using Unsafe): Chunk[Byte] = {
    chunkFromBytes(uvBuffer.base, uvBuffer.length)
  }

  inline def attempt(f: Unsafe ?=> ErrorCode): IO[IOFailure, unsafe.CInt] =
    ZIO.succeed(f).handleNativeError

  def cancelRequest(req: Req): IO[IOFailure, Unit] = {
    // LibUv sometimes returns EBUSY if the work queue isn't currently
    // in an appropriate state, but this seems to be resolved after letting the event loop run
    // a few times
    val schedule =
      retryFailures(IOFailure.ResourceBusy()) <* Schedule.recurs(1000)
    ZIO.yieldNow
      .zipRight(ZIO.succeed(req.cancel()))
      .handleNativeError
      .retry(schedule)
      .unit
  }

  def retryFailures(
      failures: IOFailure*
  ): Schedule[Any, IOFailure, IOFailure] = {
    val ordinals = failures.map(_.ordinal).toSet
    Schedule.recurWhile[IOFailure](failure => ordinals(failure.ordinal))
  }

  private[platform] def errorCodeToFailure(
      errorCode: ErrorCode
  ): IO[IOFailure, Nothing] = {
    import ErrorCodes.*
    val msg = UvUtils.errorNameAndMessage(errorCode)
    errorCode match {
      case EADDRINUSE =>
        ZIO.fail(IOFailure.AddressInUse(m = Some(msg)))
      case EADDRNOTAVAIL =>
        ZIO.fail(IOFailure.AddressNotAvailable(m = Some(msg)))
      case EAI_CANCELED | ECANCELED | EINTR =>
        ZIO.interrupt
      case EPERM | EACCES | EROFS =>
        ZIO.fail(IOFailure.NotPermitted(m = Some(msg)))
      case EAI_FAIL | EAI_PROTOCOL =>
        ZIO.fail(IOFailure.AddressLookupFailed(m = Some(msg)))
      case EAI_NODATA | EAI_NONAME =>
        ZIO.fail(IOFailure.AddressNotFound(m = Some(msg)))
      case EBUSY =>
        ZIO.fail(IOFailure.ResourceBusy(m = Some(msg)))
      case ECONNABORTED | EPIPE =>
        ZIO.fail(IOFailure.ConnectionAborted(m = Some(msg)))
      case ECONNREFUSED =>
        ZIO.fail(IOFailure.ConnectionRefused(m = Some(msg)))
      case ECONNRESET =>
        ZIO.fail(IOFailure.ConnectionReset(m = Some(msg)))
      case EEXIST =>
        ZIO.fail(IOFailure.FileExists(m = Some(msg)))
      case EFBIG =>
        ZIO.fail(IOFailure.FileTooLarge(m = Some(msg)))
      case EHOSTUNREACH | ENETDOWN | ENETUNREACH | ENONET =>
        ZIO.fail(IOFailure.HostUnreachable(m = Some(msg)))
      case EIO =>
        ZIO.fail(IOFailure.IOError(m = msg))
      case ENODEV | ENOENT =>
        ZIO.fail(IOFailure.FileNotFound(m = Some(msg)))
      case ENOSPC =>
        ZIO.fail(IOFailure.NoSpace(m = Some(msg)))
      case ENOTEMPTY =>
        ZIO.fail(IOFailure.DirectoryNotEmpty(m = Some(msg)))
      case ETIMEDOUT =>
        ZIO.fail(IOFailure.TimedOut(m = Some(msg)))
      case ETXTBSY =>
        ZIO.fail(IOFailure.TextFileBusy(m = Some(msg)))
      case EOF =>
        ZIO.fail(IOFailure.EndOfFile)
      case E2BIG | EAFNOSUPPORT | EAI_ADDRFAMILY | EAI_BADFLAGS | EAI_BADHINTS |
          EAI_FAMILY | EAI_OVERFLOW | EAI_SERVICE | EAI_SOCKTYPE | EBADF |
          ECHARSET | EDESTADDRREQ | EFAULT | EINVAL | EISCONN | EISDIR |
          EMSGSIZE | ENAMETOOLONG | ENOPROTOOPT | ENOTDIR | ENOTSOCK | ENOTSUP |
          EOVERFLOW | EPROTONOSUPPORT | EPROTOTYPE | ERANGE | ESPIPE | ESRCH |
          EXDEV | ENXIO | ENOTTY | EFTYPE | EILSEQ | ESOCKTNOSUPPORT =>
        ZIO.die(new IllegalArgumentException(msg))
      case EALREADY | ENOTCONN | EPROTO | ESHUTDOWN =>
        ZIO.die(new IllegalStateException(msg))
      case EAI_MEMORY | ELOOP | EMFILE | ENFILE | ENOBUFS | ENOMEM | EMLINK =>
        ZIO.die(new OutOfMemoryError(msg))
      case ENOSYS =>
        ZIO.die(new NotImplementedError(msg))
      case _ =>
        ZIO.die(new IOException(s"Unknown failure: $msg"))
    }
  }

}
