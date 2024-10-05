package build 

import $meta._
import $ivy.`com.lihaoyi::mill-contrib-buildinfo:$MILL_VERSION`
import com.goyeau.mill.scalafix.ScalafixModule
import com.carlosedp.aliases._
import coursier.maven.MavenRepository
import io.github.davidgregory084.TpolecatModule
import millbuild._
import millbuild.{Versions => Vers}
import millbuild.crossplatform._
import millbuild.settings._
import mill._, mill.scalalib._, mill.scalajslib._, mill.scalanativelib._, scalafmt._
import mill.scalalib.publish.PublishInfo
import mill.scalajslib.api.ModuleKind
import mill.contrib.buildinfo.BuildInfo
import de.tobiasroeser.mill.vcs.version.VcsVersion


object `package` extends RootModule {
    implicit val buildSettings: BuildSettings = interp.watchValue(MyBuild.cachedBuildSettings)
    def resolvedBuildSettings                 = T.input(MyBuild.buildSettings())
    val scala212                              = buildSettings.scala.scala212Version
    val scala213                              = buildSettings.scala.scala213Version
    val scala3x                               = buildSettings.scala.scala3xVersion

  /** The version of Scala natively supported by the toolchain. Morphir itself may provide backends that generate code for
    * other Scala versions. We may also directly cross-compile to additional Scla versions.
    */
  val morphirScalaVersion: String = interp.watchValue(buildSettings.scala.defaultVersion)

  object morphir extends Cross[MorphirModule](buildSettings.scala.crossScalaVersions) {}

  trait MorphirModule extends Cross.Module[String] with CrossPlatform {
    import DevMode._

    trait CommonZioTestModule extends TestModule.ZioTest {
      override def ivyDeps = super.ivyDeps() ++ Agg(
        Deps.dev.zio.zio,
        Deps.dev.zio.`zio-test`,
        Deps.dev.zio.`zio-test-sbt`
      )
    }

    trait MorphirCommonModule
        extends ScalaModule
        with CrossValue
        with TpolecatModule
        with CommonScalaModule
        with CommonCoursierModule {
      def semanticDbVersion = T.input(Vers.semanticDb(partialVersion()))

      def compilerPluginDependencies(selectedScalaVersion: String) =
        Agg.when(selectedScalaVersion.startsWith("3.")) {
          Agg(Deps.org.`scala-lang`.`scala3-compiler`(selectedScalaVersion))
        }
    }

    trait MorphirCommonCrossModule extends CrossPlatformScalaModule with CrossValue with CommonCrossScalaModule {
      def semanticDbVersion = T.input(Vers.semanticDb(partialVersion()))
      def compilerPluginDependencies(selectedScalaVersion: String) =
        Agg.when(selectedScalaVersion.startsWith("3.")) {
          Agg(Deps.org.`scala-lang`.`scala3-compiler`(selectedScalaVersion))
        }
    }

    trait MorphirJVMModule extends MorphirCommonCrossModule {
      def platform = Platform.JVM
    }

    trait MorphirJSModule extends MorphirCommonCrossModule with ScalaJSModule {
      import mill.scalajslib.api._
      def platform       = Platform.JS
      def scalaJSVersion = T(resolvedBuildSettings().js.version)
    }

    trait MorphirNativeModule extends MorphirCommonCrossModule with ScalaNativeModule {
      def platform           = Platform.Native
      def scalaNativeVersion = T(resolvedBuildSettings().native.version)
    }

    object ir extends CrossPlatform with CrossValue {
      trait Shared extends MorphirCommonCrossModule with MorphirPublishModule {
        def ivyDeps                                        = Agg(Deps.io.circe.`circe-core`, Deps.io.circe.`circe-generic`, Deps.io.circe.`circe-parser`)
        def platformSpecificModuleDeps: Seq[CrossPlatform] = Seq(sdk.core, sdk.json)
      }
      object jvm extends Shared with MorphirJVMModule {
        object test extends ScalaTests with CommonZioTestModule
      }

      object js extends Shared with MorphirJSModule {
        object test extends ScalaJSTests with CommonZioTestModule {
          override def ivyDeps = super.ivyDeps() ++ Agg(
            Deps.io.github.cquiroz.`scala-java-time`
          )
        }
      }

      object native extends Shared with MorphirNativeModule {
        object test extends ScalaNativeTests with CommonZioTestModule
      }
    }

    object sdk extends Module {
      object core extends CrossPlatform with CrossValue {
        trait Shared extends MorphirCommonCrossModule with MorphirPublishModule {
          def ivyDeps = Agg(Deps.org.`scala-lang`.modules.`scala-collection-compat`)
        }
        object jvm extends Shared with MorphirJVMModule {
          def ivyDeps = super.ivyDeps() ++ Agg(
            Deps.com.`47Deg`.memeid4s
          )
          object test extends ScalaTests with CommonZioTestModule
        }

        object js extends Shared with MorphirJSModule {
          object test extends ScalaJSTests with CommonZioTestModule
        }

        object native extends Shared with MorphirNativeModule {
          object test extends ScalaNativeTests with CommonZioTestModule
        }

      }

      object json extends CrossPlatform with CrossValue {
        trait Shared extends MorphirCommonCrossModule with MorphirPublishModule {

          // Making these compile time only dependencies which is the equivalent to the provided scope in Maven, but I feel we
          // should stop doing this. Need to discuss with existing consumers of the SDK to see if this is a problem.
          def compileIvyDeps = Agg(
            Deps.io.circe.`circe-core`,
            Deps.io.circe.`circe-generic`,
            Deps.io.circe.`circe-parser`
          )

          def platformSpecificModuleDeps: Seq[CrossPlatform] = Seq(sdk.core)
        }
        object jvm extends Shared with MorphirJVMModule {
          object test extends ScalaTests with CommonZioTestModule
        }

        object js extends Shared with MorphirJSModule {
          object test extends ScalaJSTests with CommonZioTestModule
        }

        object native extends Shared with MorphirNativeModule {
          object test extends ScalaNativeTests with CommonZioTestModule
        }
      }

    }
  }


  //-----------------------------------------------------------------------------
  // Build settings and common code
  //-----------------------------------------------------------------------------
  trait MorphirScalaModule extends ScalaModule with TpolecatModule with CommonCoursierModule { self =>

    override def scalacOptions = T {
      super.scalacOptions().filterNot(Set("-Xlint:nullary-override"))
    }
  }

  trait MorphirScalafixModule extends ScalafixModule

  trait MorphirPublishModule extends PublishModule with JavaModule {
    import mill.scalalib.publish._
    def packageDescription: String = s"The $artifactName package"
    override def publishVersion: T[String] = VcsVersion.vcsState().format()

    def pomSettings = PomSettings(
      description = packageDescription,
      organization = "org.morphir",
      url = "https://github.com/finos/morphir-jvm",
      licenses = Seq(License.`Apache-2.0`),
      versionControl = VersionControl.github("finos", "morphir-jvm"),
      developers = Seq(
        Developer(
          "DamianReeves",
          "Damian Reeves",
          "https://github.com/DamianReeves"
        ),
        Developer(
          "AttilaMihaly",
          "Attila Mihaly",
          "https://github.com/AttilaMihaly"
        )
      )
    )
  }

  import mill.eval.{ Evaluator, EvaluatorPaths }

  def bspInstall(jobs: Int = 1) = T.command {
    mill.bsp.BSP.install(jobs)
  }

  def idea(ev: Evaluator.AllBootstrapEvaluators) = T.command {
    mill.idea.GenIdea.idea(ev)
  }

  // def checkFormatAll(evaluator:Evaluator, sources: mill.main.Tasks[Seq[PathRef]])= T.command{
  //   ScalafmtModule.checkFormatAll(sources)()
  // }

  // With this we can now just do ./mill reformatAll __.sources
  // instead of ./mill -w mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources
  def reformatAll(evaluator: Evaluator, @mainargs.arg(short='s')sources: mill.main.Tasks[Seq[PathRef]]) = T.command {
    ScalafmtModule.reformatAll(sources)()
  }

  def showBuildSettings() = T.command {
    MyBuild.showBuildSettings()
  }

  def crossScalaVersions = T.task {
    buildSettings.scala.crossScalaVersions
  }

  // object MyAliases extends Aliases {
  //   def fmt                    = alias("mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources")
  //   def checkfmt               = alias("mill.scalalib.scalafmt.ScalafmtModule/checkFormatAll __.sources")
  //   def ciTestJS               = alias("morphir.__.js.__.compile + morphir.__.js.publishArtifacts + morphir.__.js.__.test")
  //   def ci_test_js_2_12        = morphirAlias(build.scala212)("__.js.__.compile", "__.js.publishArtifacts", "__.js.__.test")
  //   def ci_test_js_2_13        = morphirAlias(build.scala213)("__.js.__.compile", "__.js.publishArtifacts", "__.js.__.test")
  //   def ci_test_js_3           = morphirAlias(build.scala3x)("__.js.__.compile", "__.js.publishArtifacts", "__.js.__.test")
  //   def deps                   = alias("mill.scalalib.Dependency/showUpdates")
  //   def testall                = alias("__.test")
  //   def testJVM                = alias("morphir.__.jvm.__.test")
  //   def testJVMCached          = alias("morphir.__.jvm.__.testCached")
  //   def compileall             = alias("__.compile")
  //   def comptestall            = alias("__.compile", "__.test")
  //   def createPublishArtifacts = alias("morphir.__.publishArtifacts")

  //   def morphirAlias(scalaVersion: String)(tasks: String*) = alias(
  //     tasks.map(task => s"morphir[$scalaVersion].$task"): _*
  //   )

  // }

}
