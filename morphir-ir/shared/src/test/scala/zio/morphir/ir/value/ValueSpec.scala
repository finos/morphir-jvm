package zio.morphir.ir.value

import zio.Chunk
import zio.morphir.ir.Source.Location
import zio.morphir.ir.{Name, NativeFunction}
import zio.morphir.testing.MorphirBaseSpec
import zio.prelude._
import zio.test.Assertion.equalTo
import zio.test._

object ValueSpec extends MorphirBaseSpec {
  def spec: ZSpec[Environment, Any] = suite("ValueSpec") {
    suite("NativeApply")(
      test("foldLeft should work as expected with a native function application") {
        val a   = Value.Variable.Raw(Name("a"))
        val b   = Value.Variable.Raw(Name("b"))
        val sut = Value.NativeApply.Raw(NativeFunction.Addition, Chunk(a, b))
        val actual = sut.foldLeft[(Int, List[RawValue])](0 -> List.empty[RawValue]) {
          // Strip out the NativeApply node to be sure it is visited
          case ((ct, items), Value.NativeApply(attributes, function, args)) =>
            (ct + 1, Value.NativeApply(attributes, function, args) :: items)
          case ((ct, items), _) => (ct + 1, items)
        }
        assert(actual)(equalTo((3, List(sut))))
      }
    ) + suite("Unit")(
      test("foldLeft should work as expected with a unit value") {
        val givenLocation = Location(1, 42)
        val actual        = Value.Unit(givenLocation)
        assertTrue(
          actual.foldLeft(Location.home)((acc, v) => acc <> v.attributes) == givenLocation
        )
      }
    ) + suite("Variable")(
      test("foldLeft should work as expected a variable value") {
        val actual = Value.Variable.Raw(Name.fromString("foo"))
        assertTrue(
          actual.foldLeft(List.empty[RawValue])((acc, v) => v :: acc) == List(actual)
        )
      }
    )
  }
}
