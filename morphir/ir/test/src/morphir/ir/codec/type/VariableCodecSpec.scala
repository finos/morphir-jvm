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


package morphir.ir.codec.`type`

import morphir.ir.Name.name
import morphir.ir.Type.Variable
import morphir.ir.testing.JsonSpec
import org.scalactic.Good
import zio.test._
import zio.test.Assertion._

object VariableCodecSpec extends DefaultRunnableSpec with JsonSpec {
  def spec = suite("Codec:Type:Variable Spec")(
    suite("JSON - Encoding")(
      test("Should encode to a JSON array")(
        assert {
          val sut = Variable(("one", 2), name("morphir", "test", "model", "foo"))
          encodeAsJson(sut)
        }(
          equalTo(
            ujson.Arr(
              ujson.Str(Variable.Tag),
              ujson.Arr(
                ujson.Str("one"),
                ujson.Num(2)
              ),
              ujson.Arr(
                ujson.Str("morphir"),
                ujson.Str("test"),
                ujson.Str("model"),
                ujson.Str("foo")
              )
            )
          )
        )
      ),
      test("A type variable should encode to a JSON array where the first element is the tag name")(
        assert {
          val sut = Variable((), name("morphir", "test", "model", "foo"))
          encodeAsJson(sut).arr(0)
        }(
          equalTo(ujson.Str(Variable.Tag))
        )
      )
    ),
    suite("JSON - Decoding")(
      test("Should decode from a JSON Array") {
        val json = """["variable", {}, ["temp"]]"""
        assert(decodeString[Variable[Unit]](json))(equalTo(Good(Variable((), name("temp")))))
      }
    )
  )
}
