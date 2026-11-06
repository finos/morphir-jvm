package morphir.codegen.tasty

import zio.test.*

/** Verifies Scala case classes are converted to the same Morphir JSON as equivalent Elm type aliases. */
object CaseClassEquivalenceSpec extends TastyEquivalenceSuite:

  private val cases = List(
    arithmeticCase("personBasic", "PersonBasic", "person-basic"),
    arithmeticCase("personWithMethods", "PersonWithMethods", "person-with-methods"),
    arithmeticCase("personWithAgeCheck", "PersonWithAgeCheck", "person-with-age-check"),
    arithmeticCase("personWithAdjustedAge", "PersonWithAdjustedAge", "person-with-adjusted-age"),
    arithmeticCase("personWithinRange", "PersonWithinRange", "person-within-range"),
    arithmeticCase("accountBalance", "AccountBalance", "account-balance"),
    arithmeticCase("maybeAge", "MaybeAge", "maybe-age"),
    arithmeticCase("mixedRecord", "MixedRecord", "mixed-record"),
    arithmeticCase("genericBox", "GenericBox", "generic-box"),
    arithmeticCase("dualBox", "DualBox", "dual-box"),
    arithmeticMultiFileCase("dualBoxContainer", List("DualBox", "DualBoxContainer"), "dual-box-container"),
    arithmeticMultiFileCase("nestedBoxContainer", List("GenericBox", "NestedBoxContainer"), "nested-box-container")
  )

  def spec = suite("CaseClassEquivalenceSpec")(
    equivalenceSpecs(cases)(
      testName = equivalenceCase =>
        s"${equivalenceCase.caseName}: Scala case class and Elm type alias distributions are identical",
      descriptionPrefix = "case class fixture"
    )
  ) @@ TestAspect.sequential
