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
