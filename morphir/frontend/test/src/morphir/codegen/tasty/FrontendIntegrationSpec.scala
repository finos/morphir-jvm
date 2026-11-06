package morphir.codegen.tasty

import io.circe.Json
import io.circe.parser.parse
import zio.test.*

import java.nio.file.{ Files, Path, Paths }
import scala.jdk.CollectionConverters.*

object FrontendIntegrationSpec extends TastyEquivalenceSuite:
  private def convert(paths: List[Path]): String = TastyEquivalenceSuite.inspectorLock.synchronized {
    val output = Files.createTempFile("frontend-integration-", ".json")
    try
      tastyToMorphirIR(output.toString, paths.map(_.toString)*)
      Files.readString(output)
    finally Files.deleteIfExists(output)
  }

  private def moduleDefinitions(json: Json): Map[String, Json] =
    val library = json.hcursor.downField("distribution").focus.flatMap(_.asArray).get(3)
    library.hcursor
      .downField("modules")
      .focus
      .flatMap(_.asArray)
      .get
      .map { module =>
        val fields = module.asArray.get
        val name   = fields(0).asArray.get.map(_.asArray.get.map(_.asString.get).mkString).mkString(".")
        name -> fields(1).hcursor.downField("value").focus.get
      }
      .toMap

  def spec = suite("FrontendIntegrationSpec")(
    test("all example TASTy inputs produce format v3 with the expected modules and exports") {
      val root  = Paths.get(sys.props("test.examples.classes"))
      val files = Files.walk(root)
      val inputs =
        try files.iterator.asScala.filter(_.toString.endsWith(".tasty")).toList.sorted
        finally files.close()
      val json = parse(convert(inputs))
        .fold(error => throw new AssertionError(s"Example conversion produced invalid JSON: $error"), identity)
      val modules         = moduleDefinitions(json)
      val expectedModules = Set("tier", "reward", "palette", "dualbox", "person", "envelope", "currentsupportedexample")
      def exportsOf(module: String): Set[String] = modules(module).hcursor
        .downField("values")
        .focus
        .flatMap(_.asArray)
        .get
        .map(_.asArray.get.head.asArray.get.map(_.asString.get).mkString)
        .toSet
      val exports = exportsOf("currentsupportedexample")
      val expectedExports = Set(
        "greeting",
        "adjustedscore",
        "normalizedhistory",
        "projecttier",
        "rewardcode",
        "floatband",
        "defaultthresholds",
        "thresholdgroups",
        "thresholdcount",
        "incrementthresholds",
        "positivethresholds",
        "expandedthresholds",
        "thresholdtotal",
        "boundedmagnitude",
        "floataverage",
        "truncatedband",
        "collectedthresholds",
        "normalizedseqthresholds",
        "thresholdmap",
        "thresholdvalue",
        "incrementedwithfor",
        "summedpairs",
        "keepsreward",
        "sumpair",
        "firstoftriple",
        "firstofquadruple",
        "sumquadruple",
        "paletteintensity"
      )
      assertTrue(
        inputs.nonEmpty,
        json.hcursor.downField("formatVersion").as[Int] == Right(3),
        modules.keySet == expectedModules,
        exports == expectedExports,
        exportsOf("person") == Set("normalizedbonus", "isatleast", "adjustedbonus", "boundedbonus")
      )
    },
    test("unrelated package roots produce no distribution") {
      assertTrue(
        convert(
          List(scalaClassesDir.resolve("arithmetic/Add.tasty"), scalaClassesDir.resolve("a/b/c/AddOne.tasty"))
        ).isEmpty
      )
    },
    test("guarded match patterns produce no distribution") {
      assertTrue(convert(List(scalaClassesDir.resolve("unsupported/GuardedMatch.tasty"))).isEmpty)
    },
    test("sequence extractor patterns produce no distribution") {
      assertTrue(convert(List(scalaClassesDir.resolve("unsupported/SequencePattern.tasty"))).isEmpty)
    }
  ) @@ TestAspect.sequential
