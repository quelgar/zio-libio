package zio
package libio

val x: Option[String] = (??? : IOFailure.AddressInUse).message

enum IOFailure(
    val cause: Option[Throwable] = None,
    val message: Option[String] = None
) {
  case AddressInUse(
      c: Option[Throwable] = None,
      m: Option[String] = None
  ) extends IOFailure(c, m)
  case AddressLookupFailed(
      c: Option[Throwable] = None,
      m: Option[String] = None
  ) extends IOFailure(c, m)
  case AddressNotAvailable(
      c: Option[Throwable] = None,
      m: Option[String] = None
  ) extends IOFailure(c, m)
  case AddressNotFound(
      c: Option[Throwable] = None,
      m: Option[String] = None
  ) extends IOFailure(c, m)
  case ConnectionAborted(
      c: Option[Throwable] = None,
      m: Option[String] = None
  ) extends IOFailure(c, m)
  case ConnectionRefused(
      c: Option[Throwable] = None,
      m: Option[String] = None
  ) extends IOFailure(c, m)
  case ConnectionReset(
      c: Option[Throwable] = None,
      m: Option[String] = None
  ) extends IOFailure(c, m)
  case EndOfFile extends IOFailure()
  case DirectoryNotEmpty(
      c: Option[Throwable] = None,
      m: Option[String] = None
  ) extends IOFailure(c, m)
  case FileExists(
      c: Option[Throwable] = None,
      m: Option[String] = None
  ) extends IOFailure(c, m)
  case FileNotFound(
      c: Option[Throwable] = None,
      m: Option[String] = None
  ) extends IOFailure(c, m)
  case FileTooLarge(
      c: Option[Throwable] = None,
      m: Option[String] = None
  ) extends IOFailure(c, m)
  case HostUnreachable(
      c: Option[Throwable] = None,
      m: Option[String] = None
  ) extends IOFailure(c, m)
  case IOError(
      c: Option[Throwable] = None,
      m: String
  ) extends IOFailure(c, Some(m))
  case NotPermitted(
      c: Option[Throwable] = None,
      m: Option[String] = None
  ) extends IOFailure(c, m)
  case NoSpace(
      c: Option[Throwable] = None,
      m: Option[String] = None
  ) extends IOFailure(c, m)
  case ResourceBusy(
      c: Option[Throwable] = None,
      m: Option[String] = None
  ) extends IOFailure(c, m)
  case TextFileBusy(
      c: Option[Throwable] = None,
      m: Option[String] = None
  ) extends IOFailure(c, m)
  case TimedOut(
      c: Option[Throwable] = None,
      m: Option[String] = None
  ) extends IOFailure(c, m)
}

trait Instant extends Any {

  def seconds: Long

  def nanoSeconds: Long

}
