package morphir.ir.codec.value

import morphir.ir.{ LiteralValue, Value }
import morphir.ir.testing.JsonSpec
import io.circe.Json
import zio.test._
import zio.test.Assertion._

object LiteralSpec extends DefaultRunnableSpec with JsonSpec {
  def spec = suite("Value.Literal Spec")(
    suite("JSON encoding")(
      test("It should encode as a literal value") {
        val sut = Value.Literal((1, 2), LiteralValue.bool(true))
        assert(encodeAsJson(sut))(
          equalTo(
            Json.arr(
              Json.fromString(Value.Literal.Tag),
              Json.arr(Json.fromInt(1), Json.fromInt(2)),
              Json.fromBoolean(true)
            )
          )
        )
      }
    ),
    suite("JSON decoding")(
      test("Decoding a literal with no attributes") {
        val json = """["literal", null, ["bool_literal", true]]""".stripMargin

        val result = decodeString[Value.Literal[Unit, Boolean]](json)

        assert(result)(equalTo(Valid(Value.Literal((), LiteralValue.bool(true)))))
      }
    )
  )
}
