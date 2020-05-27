package morphir.ir.codec.value

import cats.data.Validated.Valid
import morphir.ir.Value
import zio.test._
import zio.test.Assertion._
import morphir.ir.testing.JsonSpec
import io.circe._

object UnitSpec extends DefaultRunnableSpec with JsonSpec {
  def spec = suite("Unit Spec")(
    suite("JSON encoding")(
      test("Unit encoding") {
        val sut = Value.Unit("Forty Two" -> 42)
        assert(encodeAsJson(sut))(
          equalTo(
            Json.arr(
              Json.fromString(Value.Unit.Tag),
              Json.arr(Json.fromString("Forty Two"), Json.fromInt(42))
            )
          )
        )
      }
    ),
    suite("JSON decoding")(
      test("Unit decoding") {
        val json = """["unit", "Tag"]"""
        assert(decodeString[Value.Unit[String]](json))(equalTo(Valid(Value.unit("Tag"))))
      }
    )
  )
}
