import sbt.Keys._
import sbt._

lazy val root = project
  .in(file("."))
  .aggregate(
    morphirIR
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
    scalaVersion := "3.0.0"
  )
