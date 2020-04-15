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

val productVersion = "0.0.1"

object morphir extends Module {
  import Versions.{scala211, scala212, scala213}
  object core extends Cross[CoreJvmModule](scala211, scala212, scala213) {}

  object cli extends Cross[CliModule](scala212, scala213) {}

  object scala extends Cross[ScalaBackendModule](scala211, scala212, scala213) {}

  object sdk extends Module {
    object core extends Cross[CoreSdkModule](scala211, scala212, scala213)
    object json extends Cross[JsonSdkModule](scala211, scala212, scala213)
  }

}

class CoreJvmModule(val crossScalaVersion: String) extends CommonJvmModule {

  def ivyDeps = Agg(
    ivy"com.lihaoyi::upickle::${Versions.upickle(crossScalaVersion)}",
    ivy"dev.zio::zio-streams::${Versions.zio}",
    ivy"dev.zio::zio-test::${Versions.zio}"
  )

  def moduleDeps = Seq(
    morphir.sdk.core(crossScalaVersion),
    morphir.sdk.json(crossScalaVersion)
  )

  override def packageDescription = "Morphir core package"

  object test extends Tests
}

class CliModule(val crossScalaVersion: String)
    extends CrossScalaModule
    with MorphirCommonModule
    with PublishModule {
  def publishVersion = productVersion

  def mainClass = Some("morphir.Main")
  def ivyDeps = Agg(
    ivy"org.rogach::scallop::${Versions.scallop}",
    ivy"dev.zio::zio-nio::${Versions.`zio-nio`}",
    ivy"dev.zio::zio-process::${Versions.`zio-process`}",
    ivy"com.github.alexarchambault::case-app-refined::${Versions.`case-app`}"
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

  def pomSettings = PublishSettings.pomSettings("Morphir CLI package")

  object test extends Tests with MorphirTestModule {}
}

class ScalaBackendModule(val crossScalaVersion: String)
    extends CrossScalaModule
    with MorphirCommonModule
    with PublishModule {
  def publishVersion = productVersion
  def ivyDeps = Agg(
    ivy"dev.zio::zio-streams::${Versions.zio}"
  )
  def moduleDeps = Seq(
    morphir.core(crossScalaVersion)
  )

  def pomSettings =
    PublishSettings.pomSettings("Morphir Scala bindings package")
}

class JsonSdkModule(val crossScalaVersion: String)
    extends CrossScalaModule
    with MorphirCommonModule
    with PublishModule {

  def publishVersion = productVersion

  def ivyDeps = Agg(
    ivy"com.lihaoyi::upickle::${Versions.upickle(crossScalaVersion)}"
  )

  def moduleDeps = Seq(
    morphir.sdk.core(crossScalaVersion)
  )

  def pomSettings = PublishSettings.pomSettings("Morphir SDK  for JSON")
  object test extends Tests with MorphirTestModule {}
}

class CoreSdkModule(val crossScalaVersion: String)
    extends CrossScalaModule
    with MorphirCommonModule
    with PublishModule {

  def publishVersion = productVersion

  def pomSettings = PublishSettings.pomSettings("Morphir SDK core")

  object test extends Tests with MorphirTestModule {
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"com.vdurmont:emoji-java:5.1.1"
    )
  }
}

trait MorphirCommonModule extends ScalafmtModule with ScalaModule {
  def repositories = super.repositories ++ Seq(
    MavenRepository("https://oss.sonatype.org/content/repositories/releases"),
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

  def platformSegment: String

  def sources = T.sources(
    millSourcePath / "scala",
    millSourcePath / s"scala-$platformSegment"
  )

}

trait CommonPublishModule extends MorphirCommonModule with PublishModule
with CrossScalaModule {
  def publishVersion = productVersion
  def packageDescription = T { artifactName() }
  def pomSettings = PomSettings(
    description = packageDescription(),
    organization = "org.morphir",
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

trait MorphirTestModule extends MorphirCommonModule with TestModule {
  def ivyDeps = Agg(
    ivy"dev.zio::zio-test::${Versions.zio}",
    ivy"dev.zio::zio-test-sbt::${Versions.zio}"
  )
  def testFrameworks =
    Seq("zio.test.sbt.ZTestFramework")
}

trait CommonJvmModule extends CommonPublishModule {
  def platformSegment = "jvm"

  def millSourcePath = super.millSourcePath / os.up
  trait Tests extends super.Tests with MorphirTestModule {
    def platformSegment = "jvm"
  }
}

trait CommonJsModule extends CommonPublishModule with ScalaJSModule {
  def platformSegment = "js"
  def crossScalaJSVersion: String
  def scalaJSVersion = crossScalaJSVersion
  def millSourcePath = super.millSourcePath / os.up / os.up
  trait Tests extends super.Tests with MorphirTestModule {
    def platformSegment = "js"
    def scalaJSVersion = crossScalaJSVersion
  }
}

object Versions {

  val scala211 = "2.11.12"
  val scala212 = "2.12.10"
  val scala213 = "2.13.1"
  val scalaJS06 = "0.6.32"
  val scalaJS1 = "1.0.0"

  val scalaJVMVersions = Seq(scala211, scala212, scala213)

  val scalaJSVersions = Seq(
    (scala212, scalaJS06),
    (scala213, scalaJS06)
  )

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

object PublishSettings {
  def pomSettings(description: String) = PomSettings(
    description = description,
    organization = "org.morphir",
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
