package morphir.codegen.tasty

import morphir.testing.MorphirBaseSpec
import zio.test.*
import scala.util.Try

object ParityCatalogSpec extends MorphirBaseSpec:
  private val includedSurfaces = ParityCatalog.includedTargetSurfaces

  private def surfaceById(id: String): ParitySurface =
    Try(ParityCatalog.surfaceById(id)).getOrElse(throw new AssertionError(s"Missing parity surface: $id"))

  private def idsOf(surfaces: List[ParitySurface]): List[String] =
    surfaces.map(_.id)

  def spec = suite("ParityCatalogSpec")(
    test("parity catalog entries have unique ids and non-empty goals") {
      val ids = ParityCatalog.allTargetSurfaces.map(_.id)

      assertTrue(
        (ids.distinct.size) == (ids.size),
        ParityCatalog.allTargetSurfaces.forall(_.goal.nonEmpty),
        ParityCatalog.allTargetSurfaces.forall(_.family.nonEmpty)
      )
    },
    test("parity catalog tracks the targeted Morphir SDK module families") {
      assertTrue(
        (ParityCatalog.targetedMorphirSdkModules) == (List(
          "Morphir.SDK.Aggregate",
          "Morphir.SDK.Decimal",
          "Morphir.SDK.Dict",
          "Morphir.SDK.Instant",
          "Morphir.SDK.Json.*",
          "Morphir.SDK.Key",
          "Morphir.SDK.LocalDate",
          "Morphir.SDK.LocalTime",
          "Morphir.SDK.ResultList",
          "Morphir.SDK.Rule",
          "Morphir.SDK.StatefulApp",
          "Morphir.SDK.UUID",
          "Morphir.SDK.Validate"
        ))
      )
    },
    test("each included parity surface maps to at least one Scala core form") {
      assertTrue(
        includedSurfaces.forall(_.scalaCoreForms.nonEmpty)
      )
    },
    test("catalog captures sugar-oriented Scala forms for high-value targets") {
      val sugaredIds = ParityCatalog.surfacesWithScalaSugars.map(_.id).toSet

      assertTrue(
        sugaredIds.contains("language-functions-lambdas"),
        sugaredIds.contains("language-patterns"),
        sugaredIds.contains("elmcore-list"),
        sugaredIds.contains("elmcore-maybe"),
        sugaredIds.contains("sdk-dict")
      )
    },
    test("parity catalog keeps Elm effect modules explicitly out of scope") {
      assertTrue(
        (ParityCatalog.excludedElmCoreModuleNames) == (List("Debug", "Platform", "Process", "Task"))
      )
    },
    test("parity catalog covers language, elm-core, Morphir SDK, and excluded layers") {
      val layers = ParityCatalog.allTargetSurfaces.map(_.layer).toSet

      assertTrue(
        layers.contains(ParityLayer.Language),
        layers.contains(ParityLayer.ElmCore),
        layers.contains(ParityLayer.MorphirSdk),
        layers.contains(ParityLayer.ExcludedElmCore)
      )
    },
    test("excluded Elm effect modules intentionally have no Scala encodings") {
      assertTrue(
        ParityCatalog.excludedElmCoreModules.forall(_.scalaCoreForms.isEmpty),
        ParityCatalog.excludedElmCoreModules.forall(_.scalaSugarForms.isEmpty)
      )
    },
    test("gap analysis distinguishes verified support from planned work") {
      assertTrue(
        (surfaceById("language-modules-packages").currentCoverage) == (ParityCoverage.Supported),
        (surfaceById("elmcore-result").currentCoverage) == (ParityCoverage.Planned),
        (surfaceById("sdk-dict").currentCoverage) == (ParityCoverage.Partial)
      )
    },
    test("every non-supported target carries a concrete gap summary") {
      assertTrue(
        ParityCatalog.gapSurfaces.nonEmpty,
        ParityCatalog.gapSurfaces.forall(_.gapSummary.nonEmpty)
      )
    },
    test("excluded surfaces stay excluded in the coverage model") {
      assertTrue(
        ParityCatalog.excludedElmCoreModules.forall(_.currentCoverage == ParityCoverage.Excluded)
      )
    },
    test("recommended gap order covers every gap exactly once") {
      assertTrue(
        (idsOf(ParityCatalog.orderedGapSurfaces).distinct) == (idsOf(ParityCatalog.orderedGapSurfaces)),
        (idsOf(ParityCatalog.orderedGapSurfaces).toSet) == (idsOf(ParityCatalog.gapSurfaces).toSet)
      )
    },
    test("next roadmap slice starts with foundational partial surfaces") {
      assertTrue(
        (idsOf(ParityCatalog.nextTenGapSurfaces)) == (List(
          "elmcore-basics-bool",
          "elmcore-comparable-equality",
          "elmcore-int-float-number",
          "language-literals",
          "language-functions-lambdas",
          "language-let-if-case",
          "language-tuples-record-access",
          "language-type-aliases-records",
          "language-custom-types",
          "language-patterns"
        ))
      )
    }
  ) @@ TestAspect.sequential
