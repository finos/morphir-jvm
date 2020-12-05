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

import morphir.ir._
import FQName.fQName
import Name.name
import morphir.ir.testing.JsonSpec
import org.scalactic.Good
import zio.test._
import zio.test.Assertion._

object ReferenceCodecSpec extends DefaultRunnableSpec with JsonSpec {
  def spec = suite("ReferenceCodec Spec")(
    suite("JSON - Decoding")(
      test("Decoding a type reference") {
        val json =
          """
            |[
            |  "reference",
            |  {},
            |  [
            |    [["morphir"], ["s", "d", "k"]],
            |    [["string"]],
            |    ["string"]
            |  ],
            |  []
            |]""".stripMargin

        val actual         = decodeString[Type.Reference[Unit]](json)
        val expectedFQName = fQName(
          path(name("morphir"), name("s", "d", "k")),
          path(name("string")),
          name("string")
        )
        val expected       = Type.Reference((), expectedFQName)
        assert(actual)(equalTo(Good(expected)))

      },
      test("Decoding a type reference as a Type") {
        val json =
          """
            |[
            |  "reference",
            |  {},
            |  [
            |    [["morphir"], ["s", "d", "k"]],
            |    [["string"]],
            |    ["string"]
            |  ],
            |  []
            |]""".stripMargin

        val actual         = decodeString[Type[Unit]](json)
        val expectedFQName = fQName(
          path(name("morphir"), name("s", "d", "k")),
          path(name("string")),
          name("string")
        )
        val expected       = Type.Reference((), expectedFQName)
        assert(actual)(equalTo(Good(expected)))

      },
      test("Decoding a type reference should fail if the tag is wrong") {
        val json =
          """
            |[
            |  "not-a-reference",
            |  {},
            |  [
            |    [["morphir"], ["s", "d", "k"]],
            |    [["string"]],
            |    ["string"]
            |  ],
            |  []
            |]""".stripMargin

        val actual         = decodeString[Type.Reference[Unit]](json)
        val expectedFQName = fQName(
          path(name("morphir"), name("s", "d", "k")),
          path(name("string")),
          name("string")
        )
        val expected       = Type.Reference((), expectedFQName)
        assert(actual)(not(equalTo(Good(expected))))

      }
    )
  )
}
