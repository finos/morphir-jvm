import BuildHelper._

inThisBuild(
  List(
    organization := "dev.zio",
    homepage := Some(url("https://zio.github.io/zio-sexpr/")),
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
    pgpSecretRing := file("/tmp/secret.asc")
  )
)

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("fix", "; all compile:scalafix test:scalafix; all scalafmtSbt scalafmtAll")
addCommandAlias("check", "; scalafmtSbtCheck; scalafmtCheckAll; compile:scalafix --check; test:scalafix --check")

addCommandAlias(
  "testJVM",
  ";zioSexprJVM/test"
)
addCommandAlias(
  "testJS",
  ";zioSexprJS/test"
)
addCommandAlias(
  "testNative",
  ";zioSexprNative/test:compile"
)

val zioVersion = "1.0.9"

lazy val root = project
  .in(file("."))
  .settings(
    publish / skip := true,
    unusedCompileDependenciesFilter -= moduleFilter("org.scala-js", "scalajs-library")
  )
  .aggregate(
    zioSexprJVM,
    zioSexprJS,
    zioSexprNative,
    docs
  )

lazy val zioSexpr = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("zio-sexpr"))
  .settings(stdSettings("zio-sexpr"))
  .settings(crossProjectSettings)
  .settings(buildInfoSettings("zio.sexpr"))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"          % zioVersion,
      "dev.zio" %% "zio-test"     % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt" % zioVersion % Test
    )
  )
  .settings(testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"))
  .enablePlugins(BuildInfoPlugin)

lazy val zioSexprJS = zioSexpr.js
  .settings(jsSettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)
  .settings(scalaJSUseMainModuleInitializer := true)

lazy val zioSexprJVM = zioSexpr.jvm
  .settings(dottySettings)
  .settings(libraryDependencies += "dev.zio" %%% "zio-test-sbt" % zioVersion % Test)
  .settings(scalaReflectTestSettings)

lazy val zioSexprNative = zioSexpr.native
  .settings(nativeSettings)

lazy val docs = project
  .in(file("zio-sexpr-docs"))
  .settings(stdSettings("zio-sexpr"))
  .settings(
    publish / skip := true,
    moduleName := "zio-sexpr-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(zioSexprJVM),
    ScalaUnidoc / unidoc / target := (LocalRootProject / baseDirectory).value / "website" / "static" / "api",
    cleanFiles += (ScalaUnidoc / unidoc / target).value,
    docusaurusCreateSite := docusaurusCreateSite.dependsOn(Compile / unidoc).value,
    docusaurusPublishGhpages := docusaurusPublishGhpages.dependsOn(Compile / unidoc).value
  )
  .dependsOn(zioSexprJVM)
  .enablePlugins(MdocPlugin, DocusaurusPlugin, ScalaUnidocPlugin)
