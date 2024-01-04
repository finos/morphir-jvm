package millbuild

import mill._, scalalib._, scalafmt._
import java.util.Properties
import scala.annotation.nowarn

trait CommonCrossScalaModule
    extends CrossScalaModule
    with CommonCoursierModule
    with CommonScalaModule
    with ScalafmtModule { self => }

trait CommonScalaModule extends ScalaModule {
  def compilerPlugins(scalaVersion: String) =
    if (isScala2(scalaVersion))
      Agg(
        ivy"org.typelevel:::kind-projector:0.13.2",
        ivy"com.olegpy::better-monadic-for:0.3.1"
      )
    else
      Agg()

  def disableFatalWarnings = T.input {
    T.env.get("DISABLE_WARNINGS_AS_ERRORS").map(_.toBoolean).getOrElse(false)
  }

  def isCIBuild = T.input {
    T.env.get("CI").map(_.toBoolean).getOrElse(false)
  }

  def isScala3(scalaVersion: String): Boolean = scalaVersion.startsWith("3.")

  def isScala2(scalaVersion: String): Boolean = scalaVersion.startsWith("2.")

  def isScala3: T[Boolean] = T {
    scalaVersion().startsWith("3.")
  }

  def isScala2: T[Boolean] = T {
    scalaVersion().startsWith("2.")
  }

  def isScala213: T[Boolean] = T {
    scalaVersion().startsWith("2.13.")
  }

  def partialVersion(version: String): Option[(Int, Int)] = {
    val partial = version.split('.').take(2)
    for {
      major    <- partial.headOption
      majorInt <- major.toIntOption
      minor    <- partial.lastOption
      minorInt <- minor.toIntOption
    } yield (majorInt, minorInt)
  }

  def partialVersion: T[Option[(Int, Int)]] = T {
    partialVersion(scalaVersion())
  }

  def optimize: T[Boolean] = T(false)

  def scalacOptions: Target[Seq[String]] = T {
    val options = scalacOptions(
      scalaVersion(),
      optimize = optimize(),
      isCIBuild = isCIBuild(),
      disableFatalWarnings = disableFatalWarnings()
    )
    super.scalacOptions() ++ options ++ additionalScalacOptions()
  }

  def scalaDocOptions = T {
    val extraOptions =
      if (isScala213()) {
        Seq("-Wconf:cat=scala3-migration:s")
      } else {
        Seq.empty
      }
    filterScala3DocOptions(super.scalaDocOptions()) ++ extraOptions
  }

  override def scalacPluginIvyDeps: Target[Agg[Dep]] = T {
    super.scalacPluginIvyDeps() ++ compilerPlugins(scalaVersion())
  }

  /// The location of user specific build properties. This is curremtly only setup to provide custom scalac options.
  /// This becomes useful when you want to temporarily enable a scalac option which is harder given mill runs a build serve/daemon.
  def userBuildProperties = T.source(T.workspace / "build.user.properties")

  def additionalScalacOptions = T {
    val propsPath = userBuildProperties().path
    if (os.exists(propsPath)) {
      try {
        val is = os.read.inputStream(propsPath)
        try {
          val props = new java.util.Properties()
          props.load(is)
          getAdditionalScalacOptions(props, partialVersion())
        } finally is.close()
      } catch {
        case e: Throwable =>
          println(s"Error reading $propsPath: ${e.getMessage}")
          Seq()
      }
    } else {
      Seq()
    }
  }

  def getAdditionalScalacOptions(props: Properties, partialVersion: Option[(Int, Int)]): Seq[String] = {
    val allProps =
      Option(props.getProperty("scalac.options.additional"))
        .map(str => str.split(' ').toSeq)
        .getOrElse(Seq.empty)
    partialVersion match {
      case None => allProps
      case Some((major, minor)) =>
        val majorProps =
          Option(props.getProperty(s"scalac.$major.x.options.additional"))
            .map(str => str.split(' ').toSeq)
            .getOrElse(Seq.empty)
        val majorMinorProps =
          Option(props.getProperty(s"scalac.$major.$minor.options.additional"))
            .map(str => str.split(" ").toSeq)
            .getOrElse(Seq.empty)
        allProps ++ majorProps ++ majorMinorProps
    }
  }

  lazy val commonCompilerOptions = Seq(
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-encoding",
    "utf-8",                         // Specify character encoding used by source files.
    "-explaintypes",                 // Explain type errors in more detail.
    "-feature",                      // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials",        // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros", // Allow macro definition (besides implementation and application)
    "-language:higherKinds",         // Allow higher-kinded types
    "-language:implicitConversions", // Allow definition of implicit functions called views
    "-unchecked",                    // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit",                   // Wrap field accessors to throw an exception on uninitialized access.
    "-Xlint:adapted-args",           // Warn if an argument list is modified to match the receiver.
    "-Xlint:constant",               // Evaluation of a constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select",     // Selecting member of DelayedInit.
    "-Xlint:doc-detached",           // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible",           // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any",              // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator",   // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-unit",           // Warn when nullary methods return Unit.
    "-Xlint:option-implicit",        // Option.apply used implicit view.
    "-Xlint:package-object-classes", // Class or object defined in package object.
    "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow",         // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align",            // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow",  // A local type parameter shadows a type already in scope.
    "-Ywarn-dead-code",              // Warn when dead code is identified.
    "-Ywarn-extra-implicit",         // Warn when more than one implicit parameter section is defined.
    "-Ywarn-numeric-widen",          // Warn when numerics are widened.
    "-Ywarn-unused:implicits",       // Warn if an implicit parameter is unused.
    // TODO: Re-enable this once we've cleaned up the codebase
    // "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
    // "-Ywarn-unused:locals", // Warn if a local definition is unused.
    "-Ywarn-unused:patvars",  // Warn if a variable bound in a pattern is unused.
    "-Ywarn-unused:privates", // Warn if a private member is unused.
    "-Ywarn-value-discard"    // Warn when non-Unit expression results are unused.
    // "-Xfatal-warnings" // Fail the compilation if there are any warnings.
  )

  lazy val compilerOptions2_12_Only =
    // These are unrecognized for Scala 2.13.
    Seq(
      "-Xfuture",                         // Turn on future language features.
      "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
      "-Xlint:nullary-override",          // Warn when non-nullary `def f()' overrides nullary `def f'.
      "-Xlint:unsound-match",             // Pattern match may not be typesafe.
      "-Yno-adapted-args",                // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
      "-Ypartial-unification",            // Enable partial unification in type constructor inference
      "-Ywarn-inaccessible",              // Warn about inaccessible types in method signatures.
      "-Ywarn-infer-any",                 // Warn when a type argument is inferred to be `Any`.
      "-Ywarn-nullary-override",          // Warn when non-nullary `def f()' overrides nullary `def f'.
      "-Ywarn-nullary-unit"               // Warn when nullary methods return Unit.
    )

  def priorTo2_13(scalaVersion: String): Boolean =
    partialVersion(scalaVersion) match {
      case Some((2, minor)) if minor < 13 => true
      case _                              => false
    }

  def targetScalacOptions(scalaVersion: String) =
    if (scalaVersion.startsWith("2.12")) Seq("-target:jvm-1.8", "-release", "8")
    else if (scalaVersion.startsWith("2.13")) Seq("-target:11")
    else if (scalaVersion.startsWith("3.")) Seq("-target:11")
    else Seq.empty // when we get Scala 4...

  def scalacOptions(
    scalaVersion: String,
    @nowarn optimize: Boolean,
    isCIBuild: Boolean,
    disableFatalWarnings: Boolean
  ) = {

    val versionParts = scalaVersion.split("\\.")
    val options = versionParts match {
      case Array("2", "12", _) =>
        filterScala2_12Options(commonCompilerOptions) ++ compilerOptions2_12_Only ++ Seq(
          "-language:existentials",
          "-Yrangepos",
          "-Xsource:3"
        )
      case Array("2", _, _) =>
        commonCompilerOptions ++ Seq(
          "-language:existentials",
          "-Yrangepos",
          "-Xsource:3",
          "-Wconf:cat=scala3-migration:s"
        )
      case Array("3", _, _) =>
        filterScala3Options(commonCompilerOptions) ++ Seq(
          // TODO: Enable later
          // "-source:3.0-migration",
          "-explain",
          "-explain-types",
          "-Xignore-scala2-macros",
          "-Yretain-trees",
          "-Wvalue-discard"
        )
      case _ =>
        Seq()
    }

    // Warnings as errors are always enabled for the CI build
    // and can be disabled by setting the DISABLE_WARNINGS_AS_ERRORS environment variable to true
    if (isCIBuild || !disableFatalWarnings)
      options ++ Seq("-Xfatal-warnings")
    else
      options
  }

  def filterScala3Options(opts: Seq[String]) =
    ("-Ykind-projector" +: opts)
      .filterNot(_.startsWith("-Xlint"))
      .filterNot(_.startsWith("-Ywarn-"))
      .filterNot(_ == "-explaintypes")
      .filterNot(_ == "-Xcheckinit")

  def filterScala3DocOptions(opts: Seq[String]) =
    opts
      .filterNot(_.startsWith("-Xfatal"))
      .filterNot(_.startsWith("-Ywarn"))
      .filterNot(_.startsWith("-W"))

  def filterScala2_12Options(opts: Seq[String]) =
    opts.filterNot(_ == "-Xlint:missing-interpolator")

  //  def compileIvyDeps = T{
  //    if(scalaVersion().startsWith("2.")) {
  //      super.compileIvyDeps() ++ Agg(
  //        com.github.ghik.`silencer-plugin`
  //      )
  //    } else {
  //      super.compileIvyDeps()
  //    }
  //  }
  //  def ivyDeps = T {
  //    if (scalaVersion().startsWith("2."))
  //      Agg(com.github.ghik.`silencer-lib`)
  //    else
  //      Agg()
  //  }
}
