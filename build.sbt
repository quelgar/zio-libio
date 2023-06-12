ThisBuild / scalaVersion := "3.3.0"

enablePlugins(ScalaNativePlugin)

// set to Debug for compilation details (Info is default)
logLevel := Level.Info

// import to add Scala Native options
import scala.scalanative.build._

val zioVer = "2.0.14"

lazy val root = crossProject(JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("."))
  .settings(
    name := "zio-libio",
    libraryDependencies += "dev.zio" %%% "zio-streams" % zioVer,
    libraryDependencies += "dev.zio" %%% "zio-test" % zioVer % Test,
    libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVer % Test,
    libraryDependencies += "dev.zio" %%% "zio-test-magnolia" % zioVer % Test
  )
  .nativeSettings(
    nativeConfig ~= { c =>
      c.withLTO(LTO.none) // thin
        .withMode(Mode.debug) // releaseFast
        .withGC(GC.immix) // commix
        .withTargetTriple("arm64-apple-macosx13.0.0")
    }
  )
