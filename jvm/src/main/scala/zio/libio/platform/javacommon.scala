package zio
package libio
package platform

import java.nio

def makeByteBuffer: UIO[nio.ByteBuffer] =
  JvmContext.current.flatMap { ctx =>
    ZIO.succeed {
      if ctx.directBuffers then {
        nio.ByteBuffer.allocateDirect(ctx.bufferSize)
      } else {
        nio.ByteBuffer.allocate(ctx.bufferSize)
      }
    }
  }
