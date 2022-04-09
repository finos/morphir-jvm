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
    ),
    scalaVersion := Scala213
  )
)
addCommandAlias("fmt", "; scalafmtSbt; scalafmt; test:scalafmt")
addCommandAlias("fix", "; all compile:scalafix test:scalafix; all scalafmtSbt scalafmtAll")
addCommandAlias(
  "check",
  "; scalafmtSbtCheck; scalafmtCheckAll; compile:scalafix --check; test:scalafix --check"
)

addCommandAlias(
  "testJVM",
  Seq("cli/test", "coreJVM/test", "irJVM/test", "sexprJVM/test", "jsonJVM/test").mkString(";", ";", ";")
)

addCommandAlias(
  "testJS",
  Seq("coreJS/test", "irJS/test", "sexprJS/test", "jsonJS/test").mkString(";", ";", ";")
)

lazy val scala213projects = Seq[ProjectReference](
  annotationJS,
  annotationJVM,
  annotationNative,
  coreJVM,
  coreJS,
  coreNative,
  irJVM,
  irJS,
  irNative,
  sexprJVM,
  sexprJS,
  jsonJVM,
  jsonJS
)

lazy val scala3projects = scala213projects ++ Seq[ProjectReference](cli)

lazy val root = project
  .in(file("."))
  .settings(
    name               := "morphir",
    publish / skip     := true,
    crossScalaVersions := Nil,
    welcomeMessage
  )
  .aggregate(scala213projects: _*)

lazy val root3 = project
  .in(file("3"))
  .settings(
    name               := "morphir_3",
    publish / skip     := true,
    crossScalaVersions := Nil,
    welcomeMessage
  )
  .aggregate(scala3projects: _*)

lazy val annotation = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("morphir-annotation"))
  .settings(stdSettings("zio-morphir-annotation"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.morphir.annotation"))
  .settings(Compile / console / scalacOptions ~= { _.filterNot(Set("-Xfatal-warnings")) })
  .settings(
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio-test" % Version.zio % Test
    )
  )
  .enablePlugins(BuildInfoPlugin)

lazy val annotationJVM = annotation.jvm
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % Version.zio % Test)
  .settings(scalaReflectTestSettings)

lazy val annotationJS = annotation.js
  .settings(jsSettings)
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % Version.zio % Test)
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val annotationNative = annotation.native
  .settings(nativeSettings)

lazy val cli = project
  .in(file("morphir-cli"))
  .settings(stdSettings("zio-morphir-cli"))
  .settings(dottySettings)
  .settings(Compile / console / scalacOptions ~= { _.filterNot(Set("-Xfatal-warnings")) })
  .dependsOn(coreJVM, irJVM, sexprJVM)
  .settings(
    scalaVersion := Scala3,
    libraryDependencies ++= Seq(
      ("dev.zio" %% "zio-cli"      % Version.`zio-cli`).exclude("dev.zio", "zio_2.13"),
      ("dev.zio" %% "zio"          % Version.zio).exclude("dev.zio", "zio_2.13"),
      ("dev.zio" %% "zio-test-sbt" % Version.zio % Test).exclude("dev.zio", "zio_2.13")
    )
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("morphir-core"))
  .settings(stdSettings("zio-morphir-core"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.morphir.core"))
  .settings(Compile / console / scalacOptions ~= { _.filterNot(Set("-Xfatal-warnings")) })
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %%% "zio"         % Version.zio,
      "dev.zio" %%% "zio-prelude" % Version.`zio-prelude`,
      "dev.zio" %%% "zio-test"    % Version.zio % Test
    )
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))
  .enablePlugins(BuildInfoPlugin)

lazy val coreJS = core.js
  .settings(jsSettings)
  .settings(dottySettings)
  .settings(scalaJSUseMainModuleInitializer := true)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % Version.zio % Test)

lazy val coreJVM = core.jvm
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % Version.zio % Test)
  .settings(scalaReflectTestSettings)

lazy val coreNative = core.native
  .settings(nativeSettings)

lazy val interpreter = crossProject(JSPlatform, JVMPlatform)
  .in(file("morphir-interpreter"))
  .dependsOn(ir, ir % "test->test")
  .settings(stdSettings("zio-morphir-interpreter"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.morphir.interpreter"))
  .settings(Compile / console / scalacOptions ~= { _.filterNot(Set("-Xfatal-warnings")) })
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-collection-compat" % Version.`scala-collection-compat`,
      "dev.zio"                %%% "zio"                     % Version.zio,
      "dev.zio"                %%% "zio-prelude"             % Version.`zio-prelude`,
      "dev.zio"                %%% "zio-test"                % Version.zio % Test
    )
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))
  .enablePlugins(BuildInfoPlugin)

lazy val interpreterJS = interpreter.js
  .settings(jsSettings)
  .settings(dottySettings)
  .settings(scalaJSUseMainModuleInitializer := true)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % Version.zio % Test)

lazy val interpreterJVM = interpreter.jvm
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % Version.zio % Test)
  .settings(scalaReflectTestSettings)

//lazy val interpreterNative = interpreter.native
//  .settings(nativeSettings)

lazy val ir = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("morphir-ir"))
  .settings(stdSettings("zio-morphir-ir"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.morphir.ir"))
  .settings(Compile / console / scalacOptions ~= { _.filterNot(Set("-Xfatal-warnings")) })
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-collection-compat" % Version.`scala-collection-compat`,
      "dev.zio"                %%% "zio"                     % Version.zio,
      "dev.zio"                %%% "zio-parser"              % Version.`zio-parser`,
      "dev.zio"                %%% "zio-prelude"             % Version.`zio-prelude`,
      "dev.zio"                %%% "zio-test"                % Version.zio % Test
    )
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))
  .enablePlugins(BuildInfoPlugin)

lazy val irJS = ir.js
  .settings(jsSettings)
  .settings(dottySettings)
  .settings(scalaJSUseMainModuleInitializer := true)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % Version.zio % Test)

lazy val irJVM = ir.jvm
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % Version.zio % Test)
  .settings(scalaReflectTestSettings)

lazy val irNative = ir.native
  .settings(nativeSettings)

lazy val lang = crossProject(JVMPlatform)
  .in(file("morphir-lang"))
  // .settings(scalaVersion := Scala3)
  .settings(stdSettings("zio-morphir-lang"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.morphir.lang"))
  .settings(Compile / console / scalacOptions ~= { _.filterNot(Set("-Xfatal-warnings")) })
  .settings(
    libraryDependencies ++= Seq(
      ("com.lihaoyi"   %%% "pprint"        % Version.pprint),
      ("org.scalameta" %%% "scalafmt-core" % Version.scalafmt)
        .excludeAll(
          (CrossVersion.partialVersion(scalaVersion.value) match {
            case Some((3, _)) =>
              Seq(
                ExclusionRule(organization = "org.scala-lang.modules", name = "scala-collection-compat_2.13"),
                ExclusionRule(organization = "com.lihaoyi", name = "sourcecode_2.13"),
                ExclusionRule(organization = "com.lihaoyi", name = "fansi_2.13")
              )
            case _ => Seq.empty[ExclusionRule]
          }): _*
        )
        .cross(CrossVersion.for3Use2_13)
    )
  )
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-collection-compat" % Version.`scala-collection-compat`,
      "dev.zio"                %%% "zio"                     % Version.zio,
      "dev.zio"                %%% "zio-parser"              % Version.`zio-parser`,
      "dev.zio"                %%% "zio-prelude"             % Version.`zio-prelude`,
      "dev.zio"                %%% "zio-test"                % Version.zio % Test
    )
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))
  .enablePlugins(BuildInfoPlugin)
  .dependsOn(ir)

lazy val langJVM = lang.jvm
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % Version.zio % Test)
  .settings(scalaReflectTestSettings)

lazy val json = crossProject(JSPlatform, JVMPlatform)
  .in(file("morphir-json"))
  .dependsOn(annotation, ir)
  .settings(stdSettings("zio-morphir-json"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.morphir.json"))
  .settings(Compile / console / scalacOptions ~= { _.filterNot(Set("-Xfatal-warnings")) })
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-collection-compat" % Version.`scala-collection-compat`,
      "dev.zio"                %%% "zio"                     % Version.zio,
      "dev.zio"                %%% "zio-json"                % Version.`zio-json`,
      "dev.zio"                %%% "zio-prelude"             % Version.`zio-prelude`,
      "dev.zio"                %%% "zio-test"                % Version.zio % Test
    )
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))
  .enablePlugins(BuildInfoPlugin)

lazy val jsonJS = json.js
  .settings(jsSettings)
  .settings(dottySettings)
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val jsonJVM = sdk.jvm
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % Version.zio % Test)
  .settings(scalaReflectTestSettings)

lazy val sdk = crossProject(JSPlatform, JVMPlatform)
  .in(file("morphir-sdk"))
  .dependsOn(annotation, ir)
  .settings(stdSettings("zio-morphir-sdk"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.morphir.sdk"))
  .settings(Compile / console / scalacOptions ~= { _.filterNot(Set("-Xfatal-warnings")) })
  .settings(
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-collection-compat" % Version.`scala-collection-compat`,
      "dev.zio"                %%% "zio"                     % Version.zio,
      "dev.zio"                %%% "zio-prelude"             % Version.`zio-prelude`,
      "dev.zio"                %%% "zio-test"                % Version.zio % Test
    )
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))
  .enablePlugins(BuildInfoPlugin)

lazy val sdkJS = sdk.js
  .settings(jsSettings)
  .settings(dottySettings)
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val sdkJVM = sdk.jvm
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % Version.zio % Test)
  .settings(scalaReflectTestSettings)

lazy val sexpr = crossProject(JSPlatform, JVMPlatform)
  .in(file("morphir-sexpr"))
  .settings(stdSettings("zio-morphir-sexpr"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.morphir.sexpr"))
  .settings(Compile / console / scalacOptions ~= { _.filterNot(Set("-Xfatal-warnings")) })
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
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))
  .enablePlugins(BuildInfoPlugin)

lazy val sexprJS = sexpr.js
  .settings(jsSettings)
  .settings(dottySettings)
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val sexprJVM = sexpr.jvm
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % Version.zio % Test)
  .settings(scalaReflectTestSettings)

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
  .dependsOn(irJVM)
  .enablePlugins(MdocPlugin, DocusaurusPlugin, ScalaUnidocPlugin)
