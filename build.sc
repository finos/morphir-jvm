import $ivy.`com.goyeau::mill-git:0.1.0-8-5ed3839`
import $ivy.`com.goyeau::mill-scalafix:0.1.3`
import $ivy.`io.github.davidgregory084::mill-tpolecat:0.1.3`
import $ivy.`com.lihaoyi::mill-contrib-bloop:$MILL_VERSION`
import com.goyeau.mill.git._
import com.goyeau.mill.scalafix.ScalafixModule
import io.github.davidgregory084._
import mill._
import mill.scalalib._
import mill.scalajslib._
import publish._
import mill.scalalib.scalafmt._
import coursier.maven.MavenRepository
import ammonite.ops._, ImplicitWd._

object Deps {
  object Versions {

    val scala211 = "2.11.12"
    val scala212 = "2.12.11"
    val scala213 = "2.13.1"
    val scalaJS06 = "0.6.32"
    val scalaJS1 = "1.0.0"

    val scalaJVMVersions = Seq(scala211, scala212, scala213)

    val scalaJSVersions = Seq(
      (scala212, scalaJS06),
      (scala213, scalaJS06)
    )

    val zio = "1.0.3"
    val zioConfig = "1.0.0-RC29"
    val zioLogging = "0.5.3"
    val zioNio = "1.0.0-RC10"
    val zioPrelude = "1.0.0-RC1"
    val zioProcess = "0.2.0"
    val newtype = "0.4.4"
    val decline = "1.2.0"
    val pprint = "0.5.9"
    val scalameta = "4.3.18"
    val directories = "11"
    val enumeratum = "1.6.1"
    val macroParadise = "2.1.1"
    val upickle = "1.1.0"
    val scalactic = "3.1.2"
    val scalaUri = "2.2.2"
    val oslib = "0.6.2"
  }
}

trait MorphirScalaModule extends ScalaModule with TpolecatModule { self =>

  override def scalacOptions = T {
    super.scalacOptions().filterNot(Set("-Xlint:nullary-override"))
  }
}

trait MorphirScalafixModule extends ScalafixModule

trait MorphirPublishModule extends GitVersionedPublishModule {
  def packageDescription = T(artifactName())
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
  def publishVersion: T[String] =
    GitVersionModule.version(withSnapshotSuffix = true)()
}

trait ScalaMacroModule extends ScalaModule {
  def crossScalaVersion: String

  def scalacOptions = super.scalacOptions().toList ++ (
    if (crossScalaVersion.startsWith("2.13")) List("-Ymacro-annotations")
    else List.empty
  )

  abstract override def scalacPluginIvyDeps =
    super.scalacPluginIvyDeps() ++
      (if (crossScalaVersion.startsWith("2.12"))
         Agg(ivy"org.scalamacros:::paradise:${Deps.Versions.macroParadise}")
       else
         Agg.empty)
}

trait MorphirCommonModule extends MorphirScalaModule with ScalafmtModule {

  def repositories = super.repositories ++ Seq(
    MavenRepository("https://oss.sonatype.org/content/repositories/releases"),
    MavenRepository("https://oss.sonatype.org/content/repositories/snapshots")
  )

  def platformSegment: String

  //def millSourcePath = super.millSourcePath / ammonite.ops.up
  def sources = T.sources(
    super
      .sources()
      .flatMap(source =>
        Seq(
          PathRef(source.path),
          PathRef(source.path / os.up / platformSegment / source.path.last)
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
    def scalaJSVersion = crossScalaJSVersion
  }
}

trait MorphirTestModule extends MorphirScalaModule with TestModule {
  def millSourcePath = super.millSourcePath / os.up

  def crossScalaVersion: String
  def platformSegment: String

  def ivyDeps = Agg(
    ivy"dev.zio::zio-test::${Deps.Versions.zio}",
    ivy"dev.zio::zio-test-junit::${Deps.Versions.zio}",
    ivy"dev.zio::zio-test-sbt::${Deps.Versions.zio}"
  )

  def testFrameworks =
    Seq("zio.test.sbt.ZTestFramework")

  def offset: os.RelPath = os.rel
  def sources = T.sources(
    super
      .sources()
      .++(
        CrossModuleBase
          .scalaVersionPaths(crossScalaVersion, s => millSourcePath / s"src-$s")
      )
      .flatMap(source =>
        Seq(
          PathRef(source.path / os.up / "test" / source.path.last),
          PathRef(
            source.path / os.up / platformSegment / "test" / source.path.last
          )
        )
      )
      .distinct
  )

  def resources = T.sources(
    super
      .resources()
      .flatMap(source =>
        Seq(
          PathRef(source.path / os.up / "test" / source.path.last)
        )
      )
      .distinct
  )
}

object morphir extends Module {
  import Deps._
  object ir extends Module {
    object jvm extends Cross[JvmMorphirIrModule](Versions.scala213)
    class JvmMorphirIrModule(val crossScalaVersion: String)
        extends CrossScalaModule
        with CommonJvmModule
        with MorphirPublishModule
        with ScalaMacroModule
        /*with MorphirScalafixModule*/ { self =>

      def artifactName = "morphir-ir"
      def ivyDeps = Agg(
        ivy"dev.zio::zio:${Versions.zio}",
        ivy"dev.zio::zio-streams:${Versions.zio}",
        ivy"com.lihaoyi::upickle:${Versions.upickle}",
        ivy"com.lihaoyi::pprint:${Versions.pprint}",
        ivy"io.lemonlabs::scala-uri:${Versions.scalaUri}",
        ivy"org.scalactic::scalactic:${Versions.scalactic}",
        ivy"io.estatico::newtype:${Versions.newtype}",
        ivy"dev.zio::zio-test::${Deps.Versions.zio}"
      )

      object test extends Tests {
        def platformSegment: String = self.platformSegment
        def crossScalaVersion = JvmMorphirIrModule.this.crossScalaVersion
      }
    }
  }

  object scala extends Module {

    object jvm extends Cross[JvmMorphirScalaModule](Versions.scala213)
    class JvmMorphirScalaModule(val crossScalaVersion: String)
        extends CrossScalaModule
        with CommonJvmModule
        with ScalaMacroModule
        with MorphirPublishModule { self =>
      def artifactName = "morphir-scala"
      def moduleDeps = Seq(morphir.ir.jvm(crossScalaVersion))

      def ivyDeps = Agg(
        ivy"org.scalameta::scalameta:${Versions.scalameta}"
      )

      object test extends Tests {
        def platformSegment: String = self.platformSegment
        def crossScalaVersion = JvmMorphirScalaModule.this.crossScalaVersion
      }
    }
  }
  object sdk extends Module {

    object core extends Module {
      object jvm
          extends Cross[JvmMorphirSdkCore](
            Versions.scala212,
            Versions.scala211,
            Versions.scala213
          )
      class JvmMorphirSdkCore(val crossScalaVersion: String)
          extends CrossScalaModule
          with CommonJvmModule
          with MorphirPublishModule { self =>

        def artifactName = "morphir-sdk-core"
        object test extends Tests {
          def platformSegment: String = self.platformSegment
          def crossScalaVersion = JvmMorphirSdkCore.this.crossScalaVersion
        }
      }
    }
  }

  object flowz extends Module {
    object jvm
        extends Cross[JvmMorphirFlowz](
          Versions.scala212,
          Versions.scala211,
          Versions.scala213
        )
    class JvmMorphirFlowz(val crossScalaVersion: String)
        extends CrossScalaModule
        with CommonJvmModule
        with MorphirPublishModule { self =>

      def artifactName = "morphir-flowz"
      def ivyDeps = Agg(
        ivy"dev.zio::zio:${Versions.zio}",
        ivy"dev.zio::zio-prelude:${Versions.zioPrelude}"
      )

      object test extends Tests {
        def platformSegment: String = self.platformSegment
        def crossScalaVersion = JvmMorphirFlowz.this.crossScalaVersion
      }
    }
  }
}
