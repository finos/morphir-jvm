import $ivy.`com.lihaoyi::mill-contrib-bloop:$MILL_VERSION`
import $ivy.`com.lihaoyi::mill-contrib-scalapblib:$MILL_VERSION`
import $file.ScalaPBSupport
import ScalaPBSupport.module._
import mill._
import mill.scalalib._
import publish._
import mill.scalalib.scalafmt._
import contrib.scalapblib._
import coursier.maven.MavenRepository
import ammonite.ops._, ImplicitWd._

val productVersion = "0.1.0"

object morphir extends Module {
  object core extends Cross[CoreModule]("2.11.12", "2.12.10", "2.13.1") {}

  object cli extends Cross[CliModule]("2.12.10", "2.13.1") {}

  object scala
      extends Cross[ScalaBackendModule]("2.11.12", "2.12.10", "2.13.1") {}

}

class CoreModule(val crossScalaVersion: String)
    extends CrossScalaModule
    with MorphirCommonModule
    with PublishModule {

  def publishVersion = productVersion
  def ivyDeps = Agg(
    ivy"com.lihaoyi::upickle:${Versions.upickle(crossScalaVersion)}",
    ivy"dev.zio::zio-streams:${Versions.zio}",
    ivy"dev.zio::zio-test:${Versions.zio}"
  ) //++ (if (crossScalaVersion.startsWith("2.11") Agg[Dep].empty, else Agg(ivy"dev.zio::zio-nio:${Versions.zio}"))

  def pomSettings = PomSettings(
    description = "Morphir core package",
    organization = "morphir",
    url = "https://github.com/MorganStanley/morphir-jvm",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github("MorganStanley", "morphir-jvm"),
    developers = Seq(
      Developer(
        "DamianReeves",
        "Damian Reeves",
        "https://github.com/DamianReeves"
      )
    )
  )

  object test extends Tests with MorphirTestModule {}
}

class CliModule(val crossScalaVersion: String)
    extends CrossScalaModule
    with MorphirCommonModule
    with PublishModule {
  def publishVersion = productVersion

  def mainClass = Some("morphir.Main")
  def ivyDeps = Agg(
    ivy"org.rogach::scallop:${Versions.scallop}",
    ivy"dev.zio::zio-nio:${Versions.`zio-nio`}",
    ivy"dev.zio::zio-process:${Versions.`zio-process`}",
    ivy"com.github.alexarchambault::case-app-refined:${Versions.`case-app`}"
  )

  def moduleDeps = Seq(
    morphir.core(crossScalaVersion)
  )

  def generatedSources = T {
    Seq(PathRef(generateBuildInfoFile(productVersion)))
  }

  def compile = T {
    generatedSources()
    super.compile()
  }

  def pomSettings = PomSettings(
    description = "Morphir CLI package",
    organization = "morphir",
    url = "https://github.com/MorganStanley/morphir-jvm",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github("MorganStanley", "morphir-jvm"),
    developers = Seq(
      Developer(
        "DamianReeves",
        "Damian Reeves",
        "https://github.com/DamianReeves"
      )
    )
  )

  object test extends Tests with MorphirTestModule {}
}

class ScalaBackendModule(val crossScalaVersion: String)
    extends CrossScalaModule
    with MorphirCommonModule
    with PublishModule {
  def publishVersion = productVersion
  def ivyDeps = Agg(
    ivy"dev.zio::zio-streams:${Versions.zio}"
  )
  def moduleDeps = Seq(
    morphir.core(crossScalaVersion)
  )

  def pomSettings = PomSettings(
    description = "Morphir Scala bindings package",
    organization = "morphir",
    url = "https://github.com/MorganStanley/morphir-jvm",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github("MorganStanley", "morphir-jvm"),
    developers = Seq(
      Developer(
        "DamianReeves",
        "Damian Reeves",
        "https://github.com/DamianReeves"
      )
    )
  )
}

trait MorphirCommonModule extends ScalafmtModule with ScalaModule {
  def repositories = super.repositories ++ Seq(
    MavenRepository("https://oss.sonatype.org/content/repositories/snapshots")
  )
  def scalacOptions = T {
    if (scalaVersion().startsWith("2.11")) {
      Seq(
        "-deprecation", // Emit warning and location for usages of deprecated APIs.
        "-explaintypes",
        "-feature",
        "-unchecked",
        "-target:jvm-1.8",
        "-language:reflectiveCalls",
        "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
        "-language:higherKinds", // Allow higher-kinded types
        "-language:implicitConversions", // Allow definition of implicit functions called views
        "-unchecked", // Enable additional warnings where generated code depends on assumptions.
        "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
        "-Xfatal-warnings" // Fail the compilation if there are any warnings.
      ) ++ Seq("-encoding", "utf-8")

    } else {
      Seq(
        "-deprecation", // Emit warning and location for usages of deprecated APIs.
        "-explaintypes",
        "-feature",
        "-unchecked",
        "-target:jvm-1.8",
        "-language:reflectiveCalls",
        "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
        "-language:higherKinds", // Allow higher-kinded types
        "-language:implicitConversions", // Allow definition of implicit functions called views
        "-unchecked", // Enable additional warnings where generated code depends on assumptions.
        "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
        "-Xfatal-warnings", // Fail the compilation if there are any warnings.
        "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
        "-Ywarn-unused:privates", // Warn if a private member is unused.
        "-Xlint:inaccessible" // Warn about inaccessible types in method signatures.
      ) ++ Seq("-encoding", "utf-8")
    }
  }
}

trait MorphirTestModule extends TestModule {
  def ivyDeps = Agg(
    ivy"org.scalatest::scalatest:3.1.1",
    ivy"dev.zio::zio-test:${Versions.zio}",
    ivy"dev.zio::zio-test-sbt:${Versions.zio}"
  )
  def testFrameworks =
    Seq("org.scalatest.tools.Framework", "zio.test.sbt.ZTestFramework")
}

object Versions {
  val `grpc-netty` = "1.27.2"
  val scalaPB = "0.10.1"
  val zio = "1.0.0-RC18-2"
  val `zio-nio` = "1.0.0-RC5"
  val `zio-process` = "0.0.2"
  val `zio-config` = "1.0.0-RC12"
  val `case-app` = "2.0.0-M13"
  val `zio-grpc` = "0.1.0+4-629d4bbe-SNAPSHOT"
  val `protoc-gen-zio` = "protoc-gen-zio"
  val refined = "0.9.13"
  val scallop = "3.4.0"
  def upickle(scalaVersion: String) =
    if (scalaVersion.startsWith("2.11")) "0.7.4" else "1.0.0"
}

def generateBuildInfoFile(
    productVersion: String = productVersion
)(implicit ctx: mill.api.Ctx.Dest, ctxLog: mill.api.Ctx.Log) = {
  val filename = "BuildInfo.scala"
  val content = s"""
  package morphir
  object BuildInfo {
    val appName = "morphir"
    val productVersion = "$productVersion"
  }
  """
  ctxLog.log.info(s"Writing $filename")
  write(ctx.dest / filename, content)
  ctx.dest / filename
}
