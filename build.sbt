import BuildHelper._
import Dependencies._
inThisBuild(
  List(
    organization := "dev.zio",
    homepage := Some(url("https://zio.github.io/zio-morphir-sexpr/")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(
        "jdegoes",
        "John De Goes",
        "john@degoes.net",
        url("http://degoes.net")
      )
    ),
    pgpPassphrase := sys.env.get("PGP_PASSWORD").map(_.toArray),
    pgpPublicRing := file("/tmp/public.asc"),
    pgpSecretRing := file("/tmp/secret.asc"),
    scalaVersion := Scala3
  )
)

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("fix", "; all compile:scalafix test:scalafix; all scalafmtSbt scalafmtAll")
addCommandAlias("check", "; scalafmtSbtCheck; scalafmtCheckAll; compile:scalafix --check; test:scalafix --check")

addCommandAlias(
  "testJVM",
  Seq("coreJVM/test", "sexprJVM/test").mkString(";", ";", ";")
)

addCommandAlias(
  "testJS",
  Seq("coreJS/test", "sexprJS/test").mkString(";", ";", ";")
)

addCommandAlias(
  "testNative",
  Seq("coreNative/test:compile", "sexprNative/test:compile").mkString(";", ";", ";")
)

lazy val root = project
  .in(file("."))
  .settings(
    publish / skip := true,
    unusedCompileDependenciesFilter -= moduleFilter("org.scala-js", "scalajs-library")
  )
  .aggregate(
    coreJVM,
    coreJS,
    coreNative,
    sexprJVM,
    sexprJS,
    sexprNative,
    docs
  )

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("core"))
  .settings(stdProjectSettings("zio-morphir-core"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.morphir.core"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio"         % Version.zio,
      "dev.zio" %%% "zio-prelude" % Version.`zio-prelude`,
      "dev.zio" %%% "zio-test"    % Version.zio % Test
    )
  )
  .enablePlugins(BuildInfoPlugin)

lazy val coreJS = core.js
  .settings(jsSettings)
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val coreJVM = core.jvm

lazy val coreNative = core.native
  .settings(nativeSettings)

lazy val sexpr = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("zio-morphir-sexpr"))
  .settings(stdProjectSettings("zio-morphir-sexpr"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.morphir.sexpr"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio"      % Version.zio,
      "dev.zio" %%% "zio-test" % Version.zio % Test
    )
  )
  .enablePlugins(BuildInfoPlugin)

lazy val sexprJS = sexpr.js
  .settings(jsSettings)
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val sexprJVM = sexpr.jvm

lazy val sexprNative = sexpr.native
  .settings(nativeSettings)

lazy val docs = project
  .in(file("zio-morphir-docs"))
  .settings(stdSettings("zio-morphir"))
  .settings(
    publish / skip := true,
    moduleName := "zio-morphir-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(sexprJVM),
    ScalaUnidoc / unidoc / target := (LocalRootProject / baseDirectory).value / "website" / "static" / "api",
    cleanFiles += (ScalaUnidoc / unidoc / target).value,
    docusaurusCreateSite := docusaurusCreateSite.dependsOn(Compile / unidoc).value,
    docusaurusPublishGhpages := docusaurusPublishGhpages.dependsOn(Compile / unidoc).value
  )
  .dependsOn(sexprJVM)
  .enablePlugins(MdocPlugin, DocusaurusPlugin, ScalaUnidocPlugin)

//------------------------------------------------------------------------------
// Settings
//------------------------------------------------------------------------------

def stdProjectSettings(prjName: String) = stdSettings(prjName) ++ Seq(
  crossScalaVersions := {
    crossProjectPlatform.value match {
      case NativePlatform => crossScalaVersions.value.distinct
      case _              => (Seq(Scala3) ++ crossScalaVersions.value).distinct
    }
  },
  ThisBuild / scalaVersion := {
    crossProjectPlatform.value match {
      case NativePlatform => scalaVersion.value
      case _              => Scala3
    }
  },
  scalacOptions ++= {
    if (scalaVersion.value == Scala3)
      Seq("-noindent")
    else
      Seq()
  },
  scalacOptions --= {
    if (scalaVersion.value == Scala3)
      Seq("-Xfatal-warnings")
    else
      Seq()
  },
  Compile / doc / sources := {
    val old = (Compile / doc / sources).value
    if (scalaVersion.value == Scala3) {
      Nil
    } else {
      old
    }
  },
  Test / parallelExecution := {
    val old = (Test / parallelExecution).value
    if (scalaVersion.value == Scala3) {
      false
    } else {
      old
    }
  },
  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
  libraryDependencies ++= {
    crossProjectPlatform.value match {
      case JSPlatform  =>
        Seq(
          "org.scala-js" % "scalajs-test-bridge_2.13" % "1.8.0"     % Test,
          "dev.zio"    %%% "zio-test-sbt"             % Version.zio % Test
        )
      case JVMPlatform =>
        {
          if (scalaVersion.value == Scala3)
            Seq(
              "org.scala-lang" % "scala-reflect" % Scala213           % Test
            )
          else
            Seq(
              "org.scala-lang" % "scala-reflect" % scalaVersion.value % Test
            )
        } ++ Seq("dev.zio" %%% "zio-test-sbt" % Version.zio % Test)
      case _           => Seq()
    }
  }
)
