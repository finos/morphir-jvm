import $ivy.`com.goyeau::mill-git:0.1.0-6-4254b37`
import $ivy.`com.goyeau::mill-scalafix:8515ae6`
import $ivy.`io.github.davidgregory084::mill-tpolecat:0.1.3`
import $ivy.`com.lihaoyi::mill-contrib-bloop:$MILL_VERSION`
import com.goyeau.mill.git.GitVersionedPublishModule
import com.goyeau.mill.scalafix.ScalafixModule
import io.github.davidgregory084.TpolecatModule
import mill._
import mill.scalalib._
import mill.scalajslib._
import publish._
import mill.scalalib.scalafmt._
import coursier.maven.MavenRepository
import ammonite.ops._, ImplicitWd._

object Deps {
  object Versions {

    val scala211  = "2.11.12"
    val scala212  = "2.12.10"
    val scala213  = "2.13.2"
    val scalaJS06 = "0.6.32"
    val scalaJS1  = "1.0.0"

    val scalaJVMVersions = Seq(scala211, scala212, scala213)

    val scalaJSVersions = Seq(
      (scala212, scalaJS06),
      (scala213, scalaJS06)
    )

    val zio           = "1.0.0-RC19"
    val zioConfig     = "1.0.0-RC18"
    val zioLogging    = "0.2.9"
    val zioNio        = "1.0.0-RC6"
    val zioProcess    = "0.0.4"
    val circe         = "0.13.0"
    val newtype       = "0.4.4"
    val decline       = "1.2.0"
    val pprint        = "0.5.9"
    val scalameta     = "4.3.10"
    val directories   = "11"
    val enumeratum    = "1.6.1"
    val macroParadise = "2.1.1"
  }
}

trait MorphirPublishModule extends GitVersionedPublishModule {}

trait MorphirCommonModule extends ScalaModule with ScalafmtModule with ScalafixModule with TpolecatModule {

  def millSourcePath = super.millSourcePath / offset
  def repositories = super.repositories ++ Seq(
    MavenRepository("https://oss.sonatype.org/content/repositories/releases"),
    MavenRepository("https://oss.sonatype.org/content/repositories/snapshots")
  )

  def platformSegment: String

  def offset: os.RelPath = os.rel
  def sources = T.sources(
    super
      .sources()
      .flatMap(source =>
        Seq(
          PathRef(source.path / os.up / source.path.last),
          PathRef(source.path / os.up / os.up / source.path.last)
        )
      )
  )

}

trait CommonJvmModule extends MorphirCommonModule {
  def platformSegment = "jvm"
  def crossScalaVersion: String

  def millSourcePath = super.millSourcePath / os.up
  trait Tests extends super.Tests with MorphirTestModule {
    def platformSegment = "jvm"
  }
}

trait CommonJsModule extends MorphirCommonModule with ScalaJSModule {
  def platformSegment = "js"
  def crossScalaJSVersion: String
  def scalaJSVersion = crossScalaJSVersion
  def millSourcePath = super.millSourcePath / os.up / os.up
  trait Tests extends super.Tests with MorphirTestModule {
    def platformSegment = "js"
    def scalaJSVersion  = crossScalaJSVersion
  }
}

trait MorphirTestModule extends ScalaModule with TestModule {
  def millSourcePath = super.millSourcePath / os.up

  def crossScalaVersion: String

  def ivyDeps = Agg(
    ivy"dev.zio::zio-test::${Deps.Versions.zio}",
    ivy"dev.zio::zio-test-sbt::${Deps.Versions.zio}"
  )

  def testFrameworks =
    Seq("zio.test.sbt.ZTestFramework")

  def offset: os.RelPath = os.rel
  def sources = T.sources(
    super
      .sources()
      .++(CrossModuleBase.scalaVersionPaths(crossScalaVersion, s => millSourcePath / s"src-$s"))
      .flatMap(source =>
        Seq(
          PathRef(source.path / os.up / "test" / source.path.last),
          PathRef(source.path / os.up / os.up / "test" / source.path.last)
        )
      )
      .distinct
  )
}

object morphir extends Module {
  import Deps._
  object ir extends Module {
    object jvm extends Cross[JvmMorphirIrModule](Versions.scala212, Versions.scala213)
    class JvmMorphirIrModule(val crossScalaVersion: String) extends CrossScalaModule with CommonJvmModule {
      def ivyDeps = Agg(
        ivy"dev.zio::zio:${Versions.zio}",
        ivy"dev.zio::zio-streams:${Versions.zio}",
        ivy"io.circe::circe-core:${Versions.circe}",
        ivy"io.circe::circe-generic:${Versions.circe}",
        ivy"io.circe::circe-parser:${Versions.circe}",
        ivy"com.beachape::enumeratum:${Versions.enumeratum}",
        ivy"com.beachape::enumeratum-circe:${Versions.enumeratum}",
        ivy"dev.zio::zio-test::${Deps.Versions.zio}"
      )

      object test extends Tests {
        def crossScalaVersion = JvmMorphirIrModule.this.crossScalaVersion
      }
    }
  }

  object scala extends Module {

    object jvm extends Cross[JvmMorphirScalaModule](Versions.scala212, Versions.scala213)
    class JvmMorphirScalaModule(val crossScalaVersion: String) extends CrossScalaModule with CommonJvmModule {
      def moduleDeps = Seq(morphir.ir.jvm(crossScalaVersion))
      def ivyDeps = Agg(
        ivy"org.scalameta::scalameta:${Versions.scalameta}"
      )

      object test extends Tests {
        def crossScalaVersion = JvmMorphirScalaModule.this.crossScalaVersion
      }
    }
  }
  object sdk extends Module {

    object core extends Module {
      object jvm extends Cross[JvmMorphirSdkCore](Versions.scala212, Versions.scala211, Versions.scala213)
      class JvmMorphirSdkCore(val crossScalaVersion: String) extends CrossScalaModule with CommonJvmModule {
        object test extends Tests {
          def crossScalaVersion = JvmMorphirSdkCore.this.crossScalaVersion
        }
      }
    }
  }

  object workspace extends Module {
    object jvm extends Cross[JvmMorphirWorkspace](Versions.scala212, Versions.scala213)
    class JvmMorphirWorkspace(val crossScalaVersion: String) extends CrossScalaModule with CommonJvmModule {
      def moduleDeps = Seq(morphir.ir.jvm(crossScalaVersion))
      def ivyDeps = Agg(
        ivy"dev.zio::zio:${Versions.zio}",
        ivy"dev.zio::zio-logging:${Versions.zioLogging}",
        ivy"dev.zio::zio-config:${Versions.zioConfig}",
        ivy"dev.zio::zio-config-magnolia:${Versions.zioConfig}",
        ivy"dev.zio::zio-config-typesafe:${Versions.zioConfig}",
        ivy"dev.zio::zio-process:${Versions.zioProcess}",
        ivy"dev.zio::zio-logging:${Versions.zioLogging}",
        ivy"io.estatico::newtype:${Versions.newtype}",
        ivy"com.monovore::decline-effect:${Versions.decline}",
        ivy"com.lihaoyi::pprint:${Versions.pprint}",
        ivy"org.scalameta::scalameta:${Versions.scalameta}"
      )

      def scalacOptions = super.scalacOptions() ++ (
        if (crossScalaVersion.startsWith("2.13")) Seq("-Ymacro-annotations") else Seq.empty
      )

      def scalacPluginIvyDeps =
        super.scalacPluginIvyDeps() ++
          (if (crossScalaVersion.startsWith("2.12"))
             Agg(ivy"org.scalamacros:::paradise:${Versions.macroParadise}")
           else
             Agg.empty)

      object test extends Tests {
        def crossScalaVersion = JvmMorphirWorkspace.this.crossScalaVersion
      }
    }
  }

  object cli extends Module {
    object jvm extends Cross[JvmMorphirCli](Versions.scala212, Versions.scala213)
    class JvmMorphirCli(val crossScalaVersion: String) extends CrossScalaModule with CommonJvmModule {
      def moduleDeps = Seq(morphir.ir.jvm(crossScalaVersion), morphir.workspace.jvm(crossScalaVersion))
      def ivyDeps = Agg(
        ivy"dev.zio::zio:${Versions.zio}",
        ivy"dev.zio::zio-logging:${Versions.zioLogging}",
        ivy"dev.zio::zio-config:${Versions.zioConfig}",
        ivy"dev.zio::zio-config-magnolia:${Versions.zioConfig}",
        ivy"dev.zio::zio-config-typesafe:${Versions.zioConfig}",
        ivy"dev.zio::zio-process:${Versions.zioProcess}",
        ivy"dev.zio::zio-logging:${Versions.zioLogging}",
        ivy"io.estatico::newtype:${Versions.newtype}",
        ivy"com.monovore::decline-effect:${Versions.decline}",
        ivy"com.lihaoyi::pprint:${Versions.pprint}",
        ivy"org.scalameta::scalameta:${Versions.scalameta}"
      )

      def scalacOptions = super.scalacOptions() ++ (
        if (crossScalaVersion.startsWith("2.13")) Seq("-Ymacro-annotations") else Seq.empty
      )

      def scalacPluginIvyDeps =
        super.scalacPluginIvyDeps() ++
          (if (crossScalaVersion.startsWith("2.12"))
             Agg(ivy"org.scalamacros:::paradise:${Versions.macroParadise}")
           else
             Agg.empty)

      object test extends Tests {
        def crossScalaVersion = JvmMorphirCli.this.crossScalaVersion
      }
    }
  }
}
//   object workspace extends Module
