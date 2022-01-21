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
      )
    ),
    pgpPassphrase := sys.env.get("PGP_PASSWORD").map(_.toArray),
    pgpPublicRing := file("/tmp/public.asc"),
    pgpSecretRing := file("/tmp/secret.asc"),
    scalaVersion  := Scala3
  )
)

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("fix", "; all compile:scalafix test:scalafix; all scalafmtSbt scalafmtAll")
addCommandAlias(
  "check",
  "; scalafmtSbtCheck; scalafmtCheckAll; compile:scalafix --check; test:scalafix --check"
)

addCommandAlias(
  "testJVM",
  Seq("cli/test", "coreJVM/test", "irJVM/test", "sexprJVM/test").mkString(";", ";", ";")
)

addCommandAlias(
  "testJS",
  Seq("coreJS/test", "irJS/test", "sexprJS/test").mkString(";", ";", ";")
)

addCommandAlias(
  "testNative",
  Seq("coreNative/test:compile", "irNative/test:compile", "sexprNative/test:compile").mkString(";", ";", ";")
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
    irJVM,
    irJS,
    irNative,
    sexprJVM,
    sexprJS,
    sexprNative,
    docs
  )

lazy val cli = project
  .in(file("morphir-cli"))
  .settings(stdProjectSettings("zio-morphir-cli"))
  .dependsOn(coreJVM, irJVM, sexprJVM)
  .settings(
    libraryDependencies ++= Seq(
      "com.github.alexarchambault" % "case-app_2.13" % "2.1.0-M12",
      "dev.zio"                   %% "zio-test"      % Version.zio,
      "dev.zio"                   %% "zio-test"      % Version.zio % Test
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

lazy val coreNative = core.native
  .settings(nativeSettings)

lazy val ir = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("morphir-ir"))
  .settings(stdCrossProjectSettings("zio-morphir-ir"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.morphir.ir"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio"         % Version.zio,
      "dev.zio" %%% "zio-prelude" % Version.`zio-prelude`,
      "dev.zio" %%% "zio-test"    % Version.zio % Test
    )
  )
  .enablePlugins(BuildInfoPlugin)

lazy val irJS = ir.js
  .settings(jsSettings)
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val irJVM = ir.jvm

lazy val irNative = ir.native
  .settings(nativeSettings)

lazy val sexpr = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("zio-morphir-sexpr"))
  .settings(stdCrossProjectSettings("zio-morphir-sexpr"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.morphir.sexpr"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio"      % Version.zio,
      "dev.zio" %%% "zio-test" % Version.zio % Test
    ),
    Compile / sourceGenerators += Def.task {
      val dir  = (Compile / sourceManaged).value
      val file = dir / "zio" / "morphir" / "sexpr" / "GeneratedTupleDecoders.scala"
      val decoders = (1 to 22).map { i =>
        val tparams   = (1 to i).map(p => s"A$p").mkString(", ")
        val implicits = (1 to i).map(p => s"A$p: SExprDecoder[A$p]").mkString(", ")
        val work = (1 to i)
          .map(p => s"val a$p = A$p.unsafeDecode(trace :+ traces($p), in)")
          .mkString("\n        Lexer.char(trace, in, ',')\n        ")
        val returns = (1 to i).map(p => s"a$p").mkString(", ")

        s"""implicit def tuple$i[$tparams](implicit $implicits): SExprDecoder[Tuple$i[$tparams]] =
           |    new SExprDecoder[Tuple$i[$tparams]] {
           |      val traces: Array[SExprError] = (0 to $i).map(SExprError.IndexedAccess(_)).toArray
           |      def unsafeDecode(trace: List[SExprError], in: RetractReader): Tuple$i[$tparams] = {
           |        Lexer.char(trace, in, '[')
           |        $work
           |        Lexer.char(trace, in, ']')
           |        Tuple$i($returns)
           |      }
           |    }""".stripMargin
      }
      IO.write(
        file,
        s"""package zio.morphir.sexpr
           |
           |import zio.morphir.sexpr.internal._
           |
           |private[sexpr] trait GeneratedTupleDecoders { this: SExprDecoder.type =>
           |  ${decoders.mkString("\n\n  ")}
           |}""".stripMargin
      )
      Seq(file)
    }.taskValue,
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
           |import zio.morphir.sexpr.internal._
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

lazy val sexprNative = sexpr.native
  .settings(nativeSettings)

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
// Settings
//------------------------------------------------------------------------------

def stdCrossProjectSettings(prjName: String) = stdSettings(prjName) ++ Seq(
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
      case JSPlatform =>
        Seq(
          "org.scala-js" % "scalajs-test-bridge_2.13" % "1.8.0"     % Test,
          "dev.zio"    %%% "zio-test-sbt"             % Version.zio % Test
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

def stdProjectSettings(prjName: String, givenScalaVersion: String = Scala3) = stdSettings(prjName) ++ Seq(
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
