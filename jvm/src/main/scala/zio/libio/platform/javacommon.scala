package zio
package libio
package platform

import java.nio

def makeByteBuffer: URIO[IOCtx, nio.ByteBuffer] =
  ZIO.serviceWithZIO[IOCtx] { ioCtx =>
    ZIO.succeed {
      if (ioCtx.jvm.directBuffers) {
        nio.ByteBuffer.allocateDirect(ioCtx.jvm.bufferSize)
      } else {
        nio.ByteBuffer.allocate(ioCtx.jvm.bufferSize)
      }
    }
  }
