package zio
package libio
package platform

final case class NativeContext(
    loop: scalauv.Loop,
    bufferSize: Int = NativeContext.DefaultBufferSize
)

object NativeContext {

  val DefaultBufferSize: Int = 4096

  val default: NativeContext =
    NativeContext(loop = scalauv.LibUv.uv_default_loop())

  private val fiberRef: FiberRef[NativeContext] = Unsafe.unsafely(
    FiberRef.unsafe.make(default, identity, (parent, _) => parent)
  )

  def current: UIO[NativeContext] = fiberRef.get

  def overrideLayer(context: NativeContext): ULayer[Unit] = {
    ZLayer.scoped(fiberRef.locallyScoped(context))
  }

  def modifyLayer(f: NativeContext => NativeContext): ULayer[Unit] = {
    ZLayer.scoped(fiberRef.locallyScopedWith(f))
  }

}
