package zio.morphir.ir

import zio.test.*
import zio.morphir.testing.MorphirBaseSpec

object ModuleSpec extends MorphirBaseSpec {
  def spec = suite("Module Spec")(
    suite("Definition")(
      test("It can be empty") {
        assertTrue(
          ModuleModule.emptyDefinition == ModuleModule.Definition.empty,
          ModuleModule.emptyDefinition.types.isEmpty,
          ModuleModule.emptyDefinition.values.isEmpty
        )
      }
    ),
    suite("Specification")(
      test("It can be empty") {
        assertTrue(
          ModuleModule.emptySpecification == ModuleModule.Specification.empty,
          ModuleModule.emptySpecification.types.isEmpty,
          ModuleModule.emptySpecification.values.isEmpty
        )
      }
    )
  )
}
