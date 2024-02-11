package zio.libio.platform

import java.util.IdentityHashMap
import scala.scalanative.unsafe.Ptr
import scala.scalanative.runtime
import zio.*

object ScalaNativePointers {

  private val marker = new Object()

  private val gcRoots = new IdentityHashMap[AnyRef, AnyRef]()

  inline def makeNativePtr(o: AnyRef): ZIO[Scope, Nothing, Ptr[Byte]] = {
    ZIO
      .succeed(unsafe.makeNativePtr(o))
      .withFinalizer(_ => ZIO.succeed(gcRoots.remove(o)))
  }

  object unsafe {

    inline def makeNativePtr(o: AnyRef)(using Unsafe): Ptr[Byte] = {
      gcRoots.put(o, marker)
      runtime.fromRawPtr(runtime.Intrinsics.castObjectToRawPtr(o))
    }

    inline def freeNativePtr(ptr: Ptr[Byte])(using Unsafe): AnyRef = {
      val obj = runtime.Intrinsics.castRawPtrToObject(runtime.toRawPtr(ptr))
      gcRoots.remove(obj)
      obj
    }

    inline def freeObject(o: AnyRef)(using Unsafe): Unit = {
      gcRoots
        .remove(o)
        .ensuring(_ ne null, "Object was not registered as a native pointer")
      ()
    }

    inline def nativePtrExists(o: AnyRef)(using Unsafe): Boolean = {
      gcRoots.containsKey(o)
    }

    inline def nativeToScala(p: Ptr[Byte])(using Unsafe): AnyRef = {
      runtime.Intrinsics
        .castRawPtrToObject(runtime.toRawPtr(p))
        .ensuring(
          gcRoots.containsKey(_),
          "Pointer was not registered as a native pointer"
        )
    }

  }
}
