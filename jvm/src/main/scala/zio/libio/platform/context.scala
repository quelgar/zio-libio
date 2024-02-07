package zio
package libio.platform

final case class JvmContext(
    directBuffers: Boolean = true,
    bufferSize: Int = JvmContext.DefaultBufferSize
)

object JvmContext {

  val DefaultBufferSize: Int = 4096

  val default: JvmContext = JvmContext()

  private val fiberRef: FiberRef[JvmContext] = Unsafe.unsafely(
    FiberRef.unsafe.make(default, identity, (parent, _) => parent)
  )

  def current: UIO[JvmContext] = fiberRef.get

  def overrideLayer(context: JvmContext): ULayer[Unit] = {
    ZLayer.scoped(fiberRef.locallyScoped(context))
  }

  def modifyLayer(f: JvmContext => JvmContext): ULayer[Unit] = {
    ZLayer.scoped(fiberRef.locallyScopedWith(f))
  }

}
