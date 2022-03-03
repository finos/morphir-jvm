import BuildHelper._
import Dependencies._
inThisBuild(
  List(
    organization := "dev.zio",
    homepage     := Some(url("https://zio.github.io/zio-morphir-sexpr/")),
    licenses     := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(
        "jdegoes",
        "John De Goes",
        "john@degoes.net",
        url("http://degoes.net")
      ),
      Developer(
        "DamianReeves",
        "Damian Reeves",
        "957246+DamianReeves@users.noreply.github.com",
        url("http://damianreeves.com")
      )
    )
  )
)

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("fix", "; all scalafmtSbt scalafmtAll")
addCommandAlias(
  "check",
  "; scalafmtSbtCheck; scalafmtCheckAll"
)

addCommandAlias(
  "testJVM",
  Seq("cli/test", "coreJVM/test", "irJVM/test", "sexprJVM/test").mkString(";", ";", ";")
)

addCommandAlias(
  "testJS",
  Seq("coreJS/test", "irJS/test", "sexprJS/test").mkString(";", ";", ";")
)

lazy val root = project
  .in(file("."))
  .settings(
    name           := "morphir",
    publish / skip := true,
    unusedCompileDependenciesFilter -= moduleFilter("org.scala-js", "scalajs-library"),
    welcomeMessage
  )
  .aggregate(
    annotationJVM,
    annotationJS,
    coreJVM,
    coreJS,
    interpreterJVM,
    interpreterJS,
    irJVM,
    irJS,
    sdkJVM,
    sdkJS,
    sexprJVM,
    sexprJS,
    docs
  )

lazy val annotation = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("morphir-annotation"))
  .settings(stdCrossProjectSettings("zio-morphir-annotation"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.morphir.annotation"))
  .enablePlugins(BuildInfoPlugin)

lazy val annotationJS = annotation.js
  .settings(jsSettings)
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val annotationJVM = annotation.jvm

lazy val cli = project
  .in(file("morphir-cli"))
  .settings(stdProjectSettings("zio-morphir-cli", Scala3))
  .dependsOn(coreJVM, irJVM, sexprJVM)
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-cli"  % Version.`zio-cli`,
      "dev.zio" %% "zio-test" % Version.zio,
      "dev.zio" %% "zio-test" % Version.zio % Test
    )
  )

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("morphir-core"))
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

lazy val interpreter = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("morphir-interpreter"))
  .dependsOn(ir, ir % "test->test")
  .settings(stdCrossProjectSettings("zio-morphir-interpreter"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.morphir.interpreter"))
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-collection-compat" % Version.`scala-collection-compat`,
      "dev.zio"               %%% "zio"                     % Version.zio,
      "dev.zio"               %%% "zio-prelude"             % Version.`zio-prelude`,
      "dev.zio"               %%% "zio-test"                % Version.zio % Test
    )
  )
  .enablePlugins(BuildInfoPlugin)

lazy val interpreterJS = interpreter.js
  .settings(jsSettings)
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val interpreterJVM = interpreter.jvm

lazy val ir = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("morphir-ir"))
  .settings(stdCrossProjectSettings("zio-morphir-ir"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.morphir.ir"))
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-collection-compat" % Version.`scala-collection-compat`,
      "dev.zio"                %%% "zio"                     % Version.zio,
      "dev.zio"                %%% "zio-parser"              % Version.`zio-parser`,
      "dev.zio"                %%% "zio-prelude"             % Version.`zio-prelude`,
      "dev.zio"                %%% "zio-test"                % Version.zio % Test
    )
  )
  .enablePlugins(BuildInfoPlugin)

lazy val irJS = ir.js
  .settings(jsSettings)
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val irJVM = ir.jvm

lazy val sdk = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("morphir-sdk"))
  .dependsOn(annotation, ir)
  .settings(stdCrossProjectSettings("zio-morphir-sdk"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.morphir.sdk"))
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-collection-compat" % Version.`scala-collection-compat`,
      "dev.zio"                %%% "zio"                     % Version.zio,
      "dev.zio"                %%% "zio-prelude"             % Version.`zio-prelude`,
      "dev.zio"                %%% "zio-test"                % Version.zio % Test
    )
  )
  .enablePlugins(BuildInfoPlugin)

lazy val sdkJS = sdk.js
  .settings(jsSettings)
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val sdkJVM = sdk.jvm

lazy val sexpr = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("morphir-sexpr"))
  .settings(stdCrossProjectSettings("zio-morphir-sexpr"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.morphir.sexpr"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio"        % Version.zio,
      "dev.zio" %%% "zio-parser" % Version.`zio-parser`,
      "dev.zio" %%% "zio-test"   % Version.zio % Test
    ),
    Compile / sourceGenerators += Def.task {
      val dir  = (Compile / sourceManaged).value
      val file = dir / "zio" / "morphir" / "sexpr" / "GeneratedTupleEncoders.scala"
      val encoders = (1 to 22).map { i =>
        val tparams   = (1 to i).map(p => s"A$p").mkString(", ")
        val implicits = (1 to i).map(p => s"A$p: SExprEncoder[A$p]").mkString(", ")
        val work = (1 to i)
          .map(p => s"A$p.unsafeEncode(t._$p, indent, out)")
          .mkString("\n        if (indent.isEmpty) out.write(',') else out.write(\", \")\n        ")

        s"""implicit def tuple$i[$tparams](implicit $implicits): SExprEncoder[Tuple$i[$tparams]] =
           |    new SExprEncoder[Tuple$i[$tparams]] {
           |      def unsafeEncode(t: Tuple$i[$tparams], indent: Option[Int], out: internal.Write): Unit = {
           |        out.write('[')
           |        $work
           |        out.write(']')
           |      }
           |    }""".stripMargin
      }
      IO.write(
        file,
        s"""package zio.morphir.sexpr
           |
           |private[sexpr] trait GeneratedTupleEncoders { this: SExprEncoder.type =>
           |  ${encoders.mkString("\n\n  ")}
           |}""".stripMargin
      )
      Seq(file)
    }.taskValue
  )
  .enablePlugins(BuildInfoPlugin)

lazy val sexprJS = sexpr.js
  .settings(jsSettings)
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val sexprJVM = sexpr.jvm

lazy val docs = project
  .in(file("zio-morphir-docs"))
  .settings(stdSettings("zio-morphir"))
  .settings(
    publish / skip := true,
    moduleName     := "zio-morphir-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(sexprJVM),
    ScalaUnidoc / unidoc / target := (LocalRootProject / baseDirectory).value / "website" / "static" / "api",
    cleanFiles += (ScalaUnidoc / unidoc / target).value,
    docusaurusCreateSite     := docusaurusCreateSite.dependsOn(Compile / unidoc).value,
    docusaurusPublishGhpages := docusaurusPublishGhpages.dependsOn(Compile / unidoc).value
  )
  .dependsOn(sexprJVM)
  .enablePlugins(MdocPlugin, DocusaurusPlugin, ScalaUnidocPlugin)

//------------------------------------------------------------------------------
// Scalafix related projects
//------------------------------------------------------------------------------
// lazy val scalafixInput = project
//   .in(file("scalafix/input"))
//   .settings(
//     scalafixSettings,
//     publish / skip := true
//   )
//   .disablePlugins(ScalafixPlugin)

// lazy val scalafixOutput = project
//   .in(file("scalafix/output"))
//   .settings(
//     scalafixSettings,
//     publish / skip := true
//   )
//   .disablePlugins(ScalafixPlugin)

// lazy val scalafixRules = project
//   .in(file("scalafix/rules"))
//   .settings(
//     scalafixSettings,
//     semanticdbEnabled := true,
//     libraryDependencies ++= Seq(
//       "ch.epfl.scala" %% "scalafix-core" % Version.scalafix,
//       "dev.zio"       %% "zio-prelude"   % Version.`zio-prelude`,
//       "dev.zio"       %% "zio-test"      % Version.zio % Test
//     )
//   )
//   .disablePlugins(ScalafixPlugin)
//   .enablePlugins(BuildInfoPlugin)

// lazy val scalafixTests = project
//   .in(file("scalafix/tests"))
//   .settings(
//     scalafixSettings,
//     publish / skip                        := true,
//     libraryDependencies += "ch.epfl.scala" % "scalafix-testkit" % Version.scalafix % Test cross CrossVersion.full,
//     scalafixTestkitOutputSourceDirectories :=
//       (scalafixOutput / Compile / unmanagedSourceDirectories).value,
//     scalafixTestkitInputSourceDirectories :=
//       (scalafixInput / Compile / unmanagedSourceDirectories).value,
//     scalafixTestkitInputClasspath :=
//       (scalafixInput / Compile / fullClasspath).value ++ (annotationJVM / Compile / fullClasspath).value
//   )
//   .enablePlugins(BuildInfoPlugin)
//   .enablePlugins(ScalafixTestkitPlugin)
//   .dependsOn(scalafixInput, scalafixRules)

//------------------------------------------------------------------------------
// Settings
//------------------------------------------------------------------------------

def stdCrossProjectSettings(prjName: String) = stdSettings(prjName) ++ Seq(
  crossScalaVersions := {
    crossProjectPlatform.value match {
      case NativePlatform => crossScalaVersions.value.distinct
      case _              => (crossScalaVersions.value :+ Scala3).distinct
    }
  },
  ThisBuild / scalaVersion := {
    crossProjectPlatform.value match {
      case NativePlatform => scalaVersion.value
      case _              => crossScalaVersions.value.head
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
      case JSPlatform =>
        Seq(
          "dev.zio" %%% "zio-test-sbt" % Version.zio % Test
        )
      case JVMPlatform =>
        {
          if (scalaVersion.value == Scala3)
            Seq(
              "org.scala-lang" % "scala-reflect" % Scala213 % Test
            )
          else
            Seq(
              "org.scala-lang" % "scala-reflect" % scalaVersion.value % Test
            )
        } ++ Seq("dev.zio" %%% "zio-test-sbt" % Version.zio % Test)
      case _ => Seq()
    }
  }
)

def stdProjectSettings(prjName: String, givenScalaVersion: String = Scala213) = stdSettings(prjName) ++ Seq(
  ThisBuild / scalaVersion := givenScalaVersion,
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

    val versionSpecificDependencies =
      if (scalaVersion.value == Scala3)
        Seq(
          "org.scala-lang" % "scala-reflect" % Scala213 % Test
        )
      else
        Seq(
          "org.scala-lang" % "scala-reflect" % scalaVersion.value % Test
        )

    versionSpecificDependencies ++ Seq("dev.zio" %%% "zio-test-sbt" % Version.zio % Test)
  }
)

lazy val scalafixSettings = List(
  scalaVersion := Scala213,
  addCompilerPlugin(scalafixSemanticdb),
  libraryDependencies ++= {
    if (scalaVersion.value.startsWith("3"))
      Nil
    else
      Seq(compilerPlugin(scalafixSemanticdb))
  },
  crossScalaVersions --= List(Scala212, Scala3),
  scalacOptions ++= List(
    "-Yrangepos",
    "-P:semanticdb:synthetics:on",
    "-Xsource:3.0"
  )
)
