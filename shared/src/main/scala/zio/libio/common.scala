package zio
package libio

enum IOFailure(cause: Option[Throwable] = None, message: Option[String] = None):
  case FileNotFound(
      cause: Option[Throwable] = None,
      message: Option[String] = None
  ) extends IOFailure(cause)
  case NotAFile(cause: Option[Throwable] = None, message: Option[String] = None)
      extends IOFailure(cause)
  case NotADirectory(
      cause: Option[Throwable] = None,
      message: Option[String] = None
  ) extends IOFailure(cause)
  case IOError(
      cause: Option[Throwable] = None,
      message: String
  ) extends IOFailure(cause, Some(message))
  case NotPermitted(
      cause: Option[Throwable] = None,
      message: Option[String] = None
  ) extends IOFailure(cause, message)
  case EndOfFile extends IOFailure()
  case IllegalInput(
      cause: Option[Throwable] = None,
      message: Option[String] = None
  ) extends IOFailure(cause, message)
  case OutOfMemory(
      cause: Option[Throwable] = None,
      message: Option[String] = None
  ) extends IOFailure(cause, message)
  case FileExists(
      cause: Option[Throwable] = None,
      message: Option[String] = None
  ) extends IOFailure(cause, message)
  case Unknown(cause: Option[Throwable] = None, message: String)
      extends IOFailure(cause, Some(message))

trait Instant extends Any {

  def seconds: Long

  def nanoSeconds: Long

}
