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

import morphir.ir.fuzzer.AllFuzzers
import morphir.ir.testing.JsonSpec
import zio.test._
import zio.test.Assertion._
import zio.test.TestAspect._
import FQName.fQName

object FQNameSpec extends DefaultRunnableSpec with JsonSpec with AllFuzzers {
  def spec =
    suite("FQNameSpec")(
      suite("Encoding and decoding")(
        test("Should encode properly - 1")(
          assert(
            compactEncode(
              fQName(
                path(name("morphir"), name("core")),
                path(name("morphir"), name("i", "r")),
                name("f", "q", "name")
              )
            )
          )(
            equalTo("""[[["morphir"],["core"]],[["morphir"],["i","r"]],["f","q","name"]]""")
          )
        ),
        testM("Should encode/decode in a well-behaved manner (roundtrips).")(
          checkM(fuzzFQName)(fqn => checkCodecIsWellBehaved(fqn))
        )
      )
    ) @@ silent
}
