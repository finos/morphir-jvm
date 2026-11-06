package morphir.codegen.tasty

import zio.test.*

/** Verifies case-class field access is converted to the same Morphir JSON as equivalent Elm record access. */
object CaseClassFieldAccessEquivalenceSpec extends TastyEquivalenceSuite:

  private val cases = List(
    arithmeticMultiFileCase(
      "personAgePlusOne",
      List("PersonAgePlusOne", "PersonAgePlusOneRecord"),
      "person-age-plus-one"
    ),
    arithmeticMultiFileCase(
      "accountBalanceWithFee",
      List("AccountBalanceWithFee", "AccountBalanceWithFeeRecord"),
      "account-balance-with-fee"
    ),
    arithmeticMultiFileCase(
      "maybeAgeSelection",
      List("MaybeAgeSelection", "MaybeAgeSelectionRecord"),
      "maybe-age-selection"
    ),
    arithmeticMultiFileCase(
      "dualBoxRightValue",
      List("DualBox", "DualBoxContainer", "DualBoxRightValue"),
      "dual-box-right-value"
    ),
    arithmeticMultiFileCase(
      "nestedBoxValue",
      List("GenericBox", "NestedBoxContainer", "NestedBoxValue"),
      "nested-box-value"
    )
  )

  def spec = suite("CaseClassFieldAccessEquivalenceSpec")(
    equivalenceSpecs(cases)(
      testName = equivalenceCase =>
        s"${equivalenceCase.caseName}: Scala case class field access and Elm record access distributions are identical",
      descriptionPrefix = "case class field-access fixture"
    )
  ) @@ TestAspect.sequential
