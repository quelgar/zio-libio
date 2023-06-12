package zio.libio

enum IOFailure(cause: Option[Throwable] = None):
  case FileNotFound(cause: Option[Throwable] = None) extends IOFailure(cause)
  case NotAFile(cause: Option[Throwable] = None) extends IOFailure(cause)
  case NotADirectory(cause: Option[Throwable] = None) extends IOFailure(cause)
  case ReadFailed(cause: Option[Throwable] = None) extends IOFailure(cause)
  case GeneralFailure(cause: Option[Throwable] = None, message: String)
      extends IOFailure(cause)

trait Instant extends Any {

  def seconds: Long

  def nanoSeconds: Long

}
