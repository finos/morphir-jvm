import BuildHelper._
import MimaSettings.mimaSettings
import explicitdeps.ExplicitDepsPlugin.autoImport.moduleFilterRemoveValue
import sbt.Keys

Global / onChangedBuildSource := ReloadOnSourceChanges

inThisBuild(
  List(
    organization := "org.morphir",
    homepage := Some(url("https://morphir.finos.org")),
    licenses := List(
      "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
    ),
    developers := List(
      Developer(
        "DamianReeves",
        "Damian Reeves",
        null,
        url("https://github.com/DamianReeves")
      )
    )
  )
)

resolvers ++= Seq(
  Resolver.mavenLocal,
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
  Resolver.jcenterRepo
)

addCommandAlias("prepare", "; fix; fmt")
addCommandAlias("fmt", "all root/scalafmtSbt root/scalafmtAll")

lazy val root = project
  .in(file("."))
  .settings(
    name := "morphir",
    publish / skip := true,
    unusedCompileDependenciesFilter -= moduleFilter(
      "org.scala-js",
      "scalajs-library"
    ),
    commonSettings,
    libraryDependencies ++= Dependencies.zioCommonDeps,
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    welcomeMessage
  )
  .aggregate(
    `morphir-ir`
  )

lazy val commonSettings = Seq(
  name := "morphir",
  version := "0.1.0",
  scalacOptions ++= Seq(
    "-deprecation",
    "-language:postfixOps",
    "-Ykind-projector",
    "-Yexplicit-nulls",
    "-source",
    "future",
    "-Xfatal-warnings"
  ) ++ Seq("-rewrite", "-indent"),
  scalaVersion := "3.0.0"
)

lazy val `morphir-ir` = project
  .in(file("./morphir-ir"))
  .settings(
    name := "morphir-ir",
    version := "0.1.0",
    scalacOptions ++= Seq(
      "-language:postfixOps",
      "-Ykind-projector",
      "-Yexplicit-nulls",
      "-source",
      "future",
      "-Xfatal-warnings"
    ),
    libraryDependencies ++= Dependencies.zioCommonDeps,
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    scalaVersion := "3.0.0"
  )
