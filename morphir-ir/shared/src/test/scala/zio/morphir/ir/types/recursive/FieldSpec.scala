package zio.morphir.ir.types.recursive

import zio.morphir.ir.StringOps
import zio.morphir.testing.MorphirBaseSpec
import zio.test._

object FieldSpec extends MorphirBaseSpec {
  def spec = suite("Field Spec")(
    suite("Field Construction")(
      test("A Field can be created from a string and TypeExpr using <:>") {
        val actual = "name" <:> Type.variable("String")
        assertTrue(
          actual == Field("name", Type.variable("String"))
        )
      },
      test("A Field can be created from a string and a Type using as") {
        val actual = "name" <:> Type.variable("String")
        assertTrue(
          actual == Field("name", Type.variable("String"))
        )
      }
    )
  )
}
