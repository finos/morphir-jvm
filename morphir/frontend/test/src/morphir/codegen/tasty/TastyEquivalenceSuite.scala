package morphir.codegen.tasty

import io.circe.Json
import io.circe.parser.parse
import morphir.testing.MorphirBaseSpec
import zio.test.*

import java.nio.file.{ Files, Path, Paths }

abstract class TastyEquivalenceSuite extends MorphirBaseSpec:

  protected final case class EquivalenceCase(
    caseName: String,
    scalaTastyPaths: List[String],
    elmProjectDir: String
  )

  protected final val scalaClassesDir: Path = Paths.get(
    sys.props("test.fixtures.scala.classes")
  )

  protected final val elmFixturesRoot: Path = Paths.get(
    sys.props("test.fixtures.elm.root")
  )

  protected final def arithmeticCase(caseName: String, scalaName: String, elmProjectDir: String): EquivalenceCase =
    EquivalenceCase(caseName, List(s"arithmetic/$scalaName.tasty"), elmProjectDir)

  protected final def arithmeticMultiFileCase(
    caseName: String,
    scalaNames: List[String],
    elmProjectDir: String
  ): EquivalenceCase =
    EquivalenceCase(caseName, scalaNames.map(name => s"arithmetic/$name.tasty"), elmProjectDir)

  protected final def scalaIR(relativeTastyPaths: List[String], label: String): Json =
    TastyEquivalenceSuite.inspectorLock.synchronized {
      withTempJsonFile("tasty2morphir-test-") { output =>
        val tastyPaths = relativeTastyPaths.map(path => scalaClassesDir.resolve(path).toString)
        tastyToMorphirIR(output.toString, tastyPaths*)
        parseJson(output, label)
      }
    }

  protected final def elmIR(elmProjectDir: String): Json =
    parseJson(elmFixturesRoot.resolve(elmProjectDir).resolve("morphir-ir.json"), s"Elm IR for $elmProjectDir")

  protected final def assertCaseMatches(equivalenceCase: EquivalenceCase): TestResult =
    assertTrue(
      scalaIR(equivalenceCase.scalaTastyPaths, s"Scala IR for ${equivalenceCase.scalaTastyPaths.mkString(", ")}") ==
        elmIR(equivalenceCase.elmProjectDir)
    )

  protected final def equivalenceSpecs(
    cases: List[EquivalenceCase]
  )(
    testName: EquivalenceCase => String,
    descriptionPrefix: String
  ): Spec[Any, Nothing] =
    suite(descriptionPrefix)(cases.map { equivalenceCase =>
      test(testName(equivalenceCase)) {
        assertCaseMatches(equivalenceCase)
      }
    }*)

  protected final def firstValueDefinition(json: Json): Json =
    (for
      distribution     <- json.hcursor.downField("distribution").focus.flatMap(_.asArray)
      library          <- distribution.lift(3)
      modules          <- library.hcursor.downField("modules").focus.flatMap(_.asArray)
      module           <- modules.headOption
      moduleFields     <- module.asArray
      moduleBody       <- moduleFields.lift(1)
      values           <- moduleBody.hcursor.downField("value").downField("values").focus.flatMap(_.asArray)
      value            <- values.headOption
      valueFields      <- value.asArray
      accessControlled <- valueFields.lift(1)
      definition       <- accessControlled.hcursor.downField("value").downField("value").focus
    yield definition).getOrElse(throw new AssertionError("Could not extract value definition from generated Scala IR"))

  private def withTempJsonFile[A](prefix: String)(use: Path => A): A =
    val output = Files.createTempFile(prefix, ".json")
    try use(output)
    finally Files.deleteIfExists(output)

  private def parseJson(path: Path, label: String): Json =
    parse(Files.readString(path)).fold(
      err => throw new AssertionError(s"Failed to parse $label: $err"),
      identity
    )

object TastyEquivalenceSuite:
  val inspectorLock = new Object
