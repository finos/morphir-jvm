import BuildHelper._
import explicitdeps.ExplicitDepsPlugin.autoImport.moduleFilterRemoveValue
import sbtcrossproject.CrossPlugin.autoImport.crossProject

inThisBuild(
  List(
    organization := "org.morphir",
    homepage := Some(url("https://morgan-stanley.github.io/morphir-jvm/")),
    licenses := List(
      "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
    ),
    developers := List(
      Developer(
        "DamianReeves",
        "Damian Reeves",
        "957246+DamianReeves@users.noreply.github.com",
        url("http://damianreeves.com")
      )
    ),
    pgpPassphrase := sys.env.get("PGP_PASSWORD").map(_.toArray),
    pgpPublicRing := file("/tmp/public.asc"),
    pgpSecretRing := file("/tmp/secret.asc"),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/morgan-stanley/morphir-jvm/"),
        "scm:git:git@github.com:morgan-stanley/morphir-jvm.git"
      )
    )
  )
)

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias(
  "check",
  "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck"
)

val zioVersion = "1.0.0-RC18-2"

lazy val root = project
  .in(file("."))
  .settings(
    skip in publish := true,
    unusedCompileDependenciesFilter -= moduleFilter(
      "org.scala-js",
      "scalajs-library"
    )
  )
  .aggregate(
    morphirSdkCoreJS,
    morphirSdkCoreJVM,
    morphirSdkJsonJS,
    morphirSdkJsonJVM,
    morphirCoreJS,
    morphirCoreJVM,
    morphirCliJS,
    morphirCliJVM
  )

lazy val morphirSdkCore = crossProject(JSPlatform, JVMPlatform)
  .in(file("morphir/sdk/core"))
  .settings(stdSettings("morphirSdkCore"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("morphir.sdk"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio" % zioVersion,
      //"io.estatico" %%% "newtype" % "0.4.3",
      "dev.zio" %%% "zio-test" % zioVersion % "test",
      "dev.zio" %%% "zio-test-sbt" % zioVersion % "test"
    )
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))

lazy val morphirSdkCoreJS = morphirSdkCore.js

lazy val morphirSdkCoreJVM = morphirSdkCore.jvm
  .settings(dottySettings)

lazy val morphirSdkJson = crossProject(JSPlatform, JVMPlatform)
  .in(file("morphir/sdk/json"))
  .dependsOn(morphirSdkCore)
  .settings(stdSettings("morphirSdkJson"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("morphir.sdk.json"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio" % zioVersion,
      "dev.zio" %%% "zio-test" % zioVersion % "test",
      "dev.zio" %%% "zio-test-sbt" % zioVersion % "test"
    )
  )
  .settings(upickleSettings("1.0.0"))
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))

lazy val morphirSdkJsonJS = morphirSdkJson.js

lazy val morphirSdkJsonJVM = morphirSdkJson.jvm
  .settings(dottySettings)

lazy val morphirCore = crossProject(JSPlatform, JVMPlatform)
  .in(file("morphir/core"))
  .dependsOn(morphirSdkCore, morphirSdkJson)
  .settings(stdSettings("morphirCore"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("morphir.core"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio" % zioVersion,
      "dev.zio" %%% "zio-test" % zioVersion,
      "dev.zio" %%% "zio-test-sbt" % zioVersion % "test"
    )
  )
  .settings(upickleSettings("1.0.0"))
  //.settings(macroExpansionSettings)
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))

lazy val morphirCoreJS = morphirCore.js

lazy val morphirCoreJVM = morphirCore.jvm
  .settings(dottySettings)
  .settings(zioNioSettings("1.0.0-RC6"))

lazy val morphirCli = crossProject(JSPlatform, JVMPlatform)
  .in(file("morphir/cli"))
  .dependsOn(morphirCore, morphirSdkJson, morphirSdkCore)
  .settings(stdSettings("morphirCli"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("morphir.cli"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % zioVersion,
      "dev.zio" %% "zio-test" % zioVersion % "test",
      "dev.zio" %% "zio-test-sbt" % zioVersion % "test"
    )
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))

lazy val morphirCliJS = morphirCli.js
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val morphirCliJVM = morphirCli.jvm
  .settings(dottySettings)

lazy val docs = project
  .in(file("morphir-jvm-docs"))
  .settings(
    skip.in(publish) := true,
    moduleName := "morphir-jvm-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % zioVersion
    ),
    unidocProjectFilter in (ScalaUnidoc, unidoc) := inProjects(root),
    target in (ScalaUnidoc, unidoc) := (baseDirectory in LocalRootProject).value / "website" / "static" / "api",
    cleanFiles += (target in (ScalaUnidoc, unidoc)).value,
    docusaurusCreateSite := docusaurusCreateSite
      .dependsOn(unidoc in Compile)
      .value,
    docusaurusPublishGhpages := docusaurusPublishGhpages
      .dependsOn(unidoc in Compile)
      .value
  )
  .dependsOn(root)
  .enablePlugins(MdocPlugin, DocusaurusPlugin, ScalaUnidocPlugin)
