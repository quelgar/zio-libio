ThisBuild / scalaVersion := "3.3.1"

enablePlugins(ScalaNativePlugin)

// set to Debug for compilation details (Info is default)
logLevel := Level.Info

// import to add Scala Native options
import scala.scalanative.build._

val zioVer = "2.0.21"

val scalaUvVer = "0.0.3-SNAPSHOT"

ThisBuild / scalacOptions ++= Seq(
  "-new-syntax",
  "-no-indent",
  "-java-output-version",
  "21",
  "-Wvalue-discard",
  "-Wunused:all",
  "-deprecation",
  "-feature"
  // "-Werror"
)

lazy val root = crossProject(JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("."))
  .settings(
    name := "zio-libio",
    libraryDependencies += "dev.zio" %%% "zio-streams" % zioVer,
    libraryDependencies += "dev.zio" %%% "zio-test" % zioVer % Test,
    libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVer % Test
    // libraryDependencies += "dev.zio" %%% "zio-test-magnolia" % zioVer % Test
  )
  .nativeSettings(
    nativeConfig ~= { c =>
      c.withLTO(LTO.none) // thin
        .withMode(Mode.debug) // releaseFast
        .withGC(GC.immix) // commix
        .withTargetTriple("arm64-apple-macosx13.0.0")
    },
    libraryDependencies += "io.github.quelgar" %%% "scala-uv" % scalaUvVer,
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.5.0"
  )
