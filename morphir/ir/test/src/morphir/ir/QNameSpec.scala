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


package morphir.ir

import morphir.ir.path.Path
import morphir.ir.QName.qName
import morphir.ir.fuzzer.AllFuzzers
import morphir.ir.testing.JsonSpec
import zio.test.TestAspect._
import zio.test._

object QNameSpec extends DefaultRunnableSpec with JsonSpec with AllFuzzers {

  def spec = suite("QNameSpec")(
    suite("Encoding/Decoding a QName")(
      test("Encoder test scenario 1 -")(
        assertEncodesToExpectedCompactJsonString(qName(morphirIRModulePath, name("name")))(
          """[[["morphir"],["i","r"]],["name"]]"""
        )
      ),
      test("Encoder test scenario 2 -")(
        assertEncodesToExpectedCompactJsonString(qName(morphirIRModulePath, name("q", "name")))(
          """[[["morphir"],["i","r"]],["q","name"]]"""
        )
      ),
      test("Encoder test scenario 3 - ")(
        assertEncodesToExpectedCompactJsonString(qName(morphirIRAdvancedModulePath, name("type")))(
          """[[["morphir"],["i","r"],["advanced"]],["type"]]"""
        )
      ),
      testM("should work in a well-behaved manner")(
        checkM(fuzzQName)(checkCodecIsWellBehaved(_))
      ) @@ silent
    )
  )

  val morphirIRModulePath: Path =
    Path.fromNames(
      name("morphir"),
      name("i", "r")
    )

  val morphirIRAdvancedModulePath: Path =
    Path.fromNames(
      name("morphir"),
      name("i", "r"),
      name("advanced")
    )
}
