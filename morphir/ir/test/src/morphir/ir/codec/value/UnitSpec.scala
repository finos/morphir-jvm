package morphir.ir.codec.value

import morphir.ir.testing.JsonSpec
import morphir.ir.Value
import org.scalactic.Good
import zio.test._
import zio.test.Assertion._

object UnitSpec extends DefaultRunnableSpec with JsonSpec {
  def spec = suite("Unit Spec")(
    suite("JSON encoding")(
      test("Unit encoding") {
        val sut = Value.Unit("Forty Two" -> 42)
        assert(encodeAsJson(sut))(
          equalTo(
            ujson.Arr(
              ujson.Str(Value.Unit.Tag),
              ujson.Arr(ujson.Str("Forty Two"), ujson.Num(42))
            )
          )
        )
      }
    ),
    suite("JSON decoding")(
      test("Unit decoding") {
        val json = """["unit", "Tag"]"""
        assert(decodeString[Value.Unit[String]](json))(equalTo(Good(Value.unit("Tag"))))
      }
    )
  )
}
