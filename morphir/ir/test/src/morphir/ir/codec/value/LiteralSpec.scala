/*
Copyright 2020 Morgan Stanley

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/


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
