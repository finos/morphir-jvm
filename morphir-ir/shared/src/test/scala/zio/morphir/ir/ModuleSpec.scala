package zio.morphir.ir

import zio.test.*
import zio.morphir.testing.MorphirBaseSpec

object ModuleSpec extends MorphirBaseSpec {
  def spec = suite("Module Spec")(
    suite("Definition")(
      test("It can be empty") {
        assertTrue(
          Module.emptyDefinition == Module.Definition.empty,
          Module.emptyDefinition.types.isEmpty,
          Module.emptyDefinition.values.isEmpty
        )
      }
    ),
    suite("Specification")(
      test("It can be empty") {
        assertTrue(
          Module.emptySpecification == Module.Specification.empty,
          Module.emptySpecification.types.isEmpty,
          Module.emptySpecification.values.isEmpty
        )
      }
    )
  )
}
