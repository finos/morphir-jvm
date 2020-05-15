import BuildHelper._
import Dependencies._
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
    pgpPublicRing := Path.userHome / "tmp" / "public.asc",
    pgpSecretRing := Path.userHome / "tmp" / "secret.asc",
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
addCommandAlias(
  "testSdkCore",
  ";  +morphirSdkCoreJVM/test ; morphirSdkCoreJS/test"
)

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
    morphirIRJVM,
    morphirIRJS,
    morphirCliJVM
  )

lazy val morphirSdkCore = crossProject(JSPlatform, JVMPlatform)
  .in(file("morphir/sdk/core"))
  .settings(stdSettings("morphir-sdk-core"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("morphir.sdk.core"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio"          % Versions.zio,
      "dev.zio" %%% "zio-test"     % Versions.zio % "test",
      "dev.zio" %%% "zio-test-sbt" % Versions.zio % "test"
    )
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))

lazy val morphirSdkCoreJS = morphirSdkCore.js
  .settings(testJsSettings)

lazy val morphirSdkCoreJVM = morphirSdkCore.jvm
  .settings(dottySettings)

lazy val morphirIR = crossProject(JVMPlatform, JSPlatform)
  .in(file("morphir/ir"))
  .settings(stdSettings("morphir-ir", Some(Seq(ScalaVersions.Scala212, ScalaVersions.Scala213))))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("morphir.ir"))
  .settings(
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "scalameta"     % "4.3.10",
      "dev.zio"       %%% "zio-streams"   % Versions.zio,
      "io.circe"      %%% "circe-core"    % Versions.circe,
      "io.circe"      %%% "circe-generic" % Versions.circe,
      "io.circe"      %%% "circe-parser"  % Versions.circe,
      "dev.zio"       %%% "zio-test"      % Versions.zio,
      "dev.zio"       %%% "zio-test-sbt"  % Versions.zio % "test"
    )
  )
  .settings(enumeratumSettings())
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))

lazy val morphirIRJVM = morphirIR.jvm
  .settings(dottySettings)
//.settings(zioNioSettings("1.0.0-RC6"))

lazy val morphirIRJS = morphirIR.js
  .settings(testJsSettings)

lazy val morphirCli = crossProject(JVMPlatform)
  .in(file("morphir/cli"))
  .dependsOn(morphirIR)
  .settings(stdSettings("morphir-cli", Some(Seq(ScalaVersions.Scala213))))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("org.morphir.cli"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio"      %% "zio"                 % Versions.zio,
      "dev.zio"      %% "zio-test"            % Versions.zio % "test",
      "dev.zio"      %% "zio-test-sbt"        % Versions.zio % "test",
      "dev.zio"      %% "zio-logging"         % "0.2.8",
      "dev.zio"      %% "zio-config"          % "1.0.0-RC17",
      "dev.zio"      %% "zio-config-magnolia" % "1.0.0-RC17",
      "dev.zio"      %% "zio-process"         % "0.0.3",
      "dev.zio"      %% "zio-logging"         % "0.2.8",
      "io.estatico"  %% "newtype"             % "0.4.4",
      "com.monovore" %% "decline-effect"      % "1.2.0",
      "com.lihaoyi"  %% "pprint"              % "0.5.9"
    )
  )
  .settings(macroExpansionSettings)
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))

lazy val morphirCliJVM = morphirCli.jvm.settings(
  libraryDependencies ++= Seq(
    "io.github.soc" % "directories" % "11"
  )
)

lazy val docs = project
  .in(file("morphir-jvm-docs"))
  .settings(
    skip.in(publish) := true,
    moduleName := "morphir-jvm-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % Versions.zio
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
//lazy val morphirSdkJson = crossProject(JSPlatform, JVMPlatform)
//  .in(file("morphir/sdk/json"))
//  .dependsOn(morphirSdkCore)
//  .settings(stdSettings("morphirSdkJson"))
//  .settings(crossProjectSettings)
//  .settings(buildInfoSettings("morphir.sdk.json"))
//  .settings(
//    libraryDependencies ++= Seq(
//      "dev.zio" %%% "zio" % Versions.zio,
//      "io.circe" %%% "circe-generic" % Versions.circe,
//      "io.circe" %%% "circe-parser" % Versions.circe,
//      "dev.zio" %%% "zio-test" % Versions.zio % "test",
//      "dev.zio" %%% "zio-test-sbt" % Versions.zio % "test"
//    )
//  )
//  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))

//lazy val morphirSdkJsonJS = morphirSdkJson.js
//
//lazy val morphirSdkJsonJVM = morphirSdkJson.jvm
//  .settings(dottySettings)

// Remove all additional repository other than Maven Central from POM
//ThisBuild / pomIncludeRepository := { _ => false }
////ThisBuild / publishTo := {
////  val nexus = "https://oss.sonatype.org/"
////  if (isSnapshot.value)
////    Some("snapshots" at nexus + "content/repositories/snapshots")
////  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
////}
//ThisBuild / publishTo := sonatypePublishToBundle.value
//ThisBuild / publishMavenStyle := true

// gpg --keyserver pool.sks-keyservers.net --send-keys F0D9DBF7DD76AD6FB9E51E0930E64443530F4AA0
