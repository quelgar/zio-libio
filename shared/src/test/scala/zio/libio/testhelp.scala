package zio.libio

import zio.*
import zio.test.*

extension [R](spec: Spec[R & IOCtx, Any])
  def usingIOCtx(
      ioCtx: IOCtx = IOCtx.Default
  ): Spec[R, Any] =
    spec
      .transform[R & IOCtx, Any] {
        case Spec.TestCase(test, annotations) =>
          Spec.TestCase(ioCtx.use(test), annotations)
        case other => other
      }
      .provideSomeEnvironment[R](_.add(ioCtx))
