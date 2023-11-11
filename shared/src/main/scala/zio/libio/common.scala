package zio
package libio

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

final case class IOCtx(jvm: IOCtx.Jvm, native: IOCtx.Native) {

  import IOCtx.*

  def withJvm(jvm: IOCtx.Jvm): IOCtx = copy(jvm = jvm)

  def withNative(native: IOCtx.Native): IOCtx = copy(native = native)

  def use[R, E, A](workflow: ZIO[R & IOCtx, E, A]): ZIO[R, E, A] = {
    if (internal.Platform.isJVM) {
      val workflowWithCtx = workflow.provideSomeEnvironment[R](_.add(this))
      jvm.executor match {
        case ExecutorType.Blocking =>
          ZIO.blocking(workflowWithCtx)
        case ExecutorType.Unchanged =>
          workflowWithCtx
        case IOCtx.ExecutorType.Custom(executor) =>
          workflowWithCtx.onExecutor(executor)
      }
    } else if (internal.Platform.isNative) {
      ???
    } else {
      ZIO.dieMessage("Unsupported platform")
    }
  }

  def scoped[R, E, A](workflow: ZIO[R & IOCtx & Scope, E, A]): ZIO[R, E, A] =
    ZIO.scoped(use(workflow))

  def layer: ULayer[IOCtx] = ZLayer.scoped {
    val getExecutor = if internal.Platform.isJVM then {
      jvm.executor match {
        case ExecutorType.Blocking =>
          ZIO.blockingExecutor
        case ExecutorType.Unchanged =>
          ZIO.executor
        case IOCtx.ExecutorType.Custom(executor) =>
          ZIO.succeed(executor)
      }
    } else if internal.Platform.isNative then {
      ???
    } else {
      ZIO.dieMessage("Unsupported platform")
    }
    getExecutor.flatMap(e => ZIO.onExecutorScoped(e)).as(this)
  }

}

object IOCtx {

  enum ExecutorType {
    case Blocking
    case Unchanged
    case Custom(executor: Executor)
  }

  final case class Jvm(
      directBuffers: Boolean = true,
      bufferSize: Int = 4096,
      executor: ExecutorType = ExecutorType.Blocking
  )

  final case class Native(
      bufferSize: Int = 4096,
      fileExecutor: ExecutorType = ExecutorType.Blocking
  )

  val Default: IOCtx = IOCtx(IOCtx.Jvm(), IOCtx.Native())

}
