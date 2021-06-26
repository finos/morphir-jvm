import sbt.Keys._
import sbt._

resolvers ++= Seq(
  Resolver.mavenLocal,
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
  Resolver.jcenterRepo
)

lazy val root = (project in file("."))
  .settings(
    commonSettings,
    libraryDependencies ++= Dependencies.zioCommonDeps,
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  )
  .aggregate(
    morphirIR
  )

lazy val commonSettings = Seq(
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
    scalaVersion := "3.0.0"
)

lazy val morphirIR = project
  .in(file("./ir"))
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
