package morphir.ir.codec.value

import morphir.ir.literal
import morphir.ir.testing.JsonSpec
import morphir.ir.Value
import Value.Literal
import org.scalactic.Good
import zio.test._
import zio.test.Assertion._

object LiteralSpec extends DefaultRunnableSpec with JsonSpec {
  def spec = suite("Codec: Value.Literal Spec")(
    suite("JSON encoding")(
      test("It should encode as a literal value") {
        val sut = Value.Literal((1, 2), literal.bool(true))
        assert(encodeAsJson(sut))(
          equalTo(
            ujson.Arr(
              ujson.Str(Value.Literal.Tag),
              ujson.Arr(ujson.Num(1), ujson.Num(2)),
              ujson.Arr(ujson.Str(literal.Literal.BoolLiteral.Tag), ujson.Bool(true))
            )
          )
        )
      }
    ),
    suite("JSON decoding")(
      test("Decoding a literal with no attributes") {
        val json   = """["literal", {}, ["bool_literal", true]]""".stripMargin
        val result = decodeString[Literal[scala.Unit]](json)

        assert(result)(equalTo(Good(Value.literal((), true))))
      }
    )
  )
}
