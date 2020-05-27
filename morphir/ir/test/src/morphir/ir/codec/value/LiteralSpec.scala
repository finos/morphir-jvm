package morphir.ir.codec.value

import morphir.ir.Value
import Value.Literal
import morphir.ir.testing.JsonSpec
import io.circe._
import zio.test._
import zio.test.Assertion._

object LiteralSpec extends DefaultRunnableSpec with JsonSpec {
  def spec = suite("Value.Literal Spec")(
    suite("JSON encoding")(
      test("It should encode as a literal value") {
        val sut = Value.literal("Hello", true)
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
        val json   = """["literal", null, ["bool_literal", true]]""".stripMargin
        val result = decodeString[Literal[Unit]](json)

        assert(result)(equalTo(Valid(Value.literal((), true))))
      }
    )
  )
}
