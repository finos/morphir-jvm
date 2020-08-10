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

import morphir.ir.documented._
import morphir.ir.testing.JsonSpec
import zio.test._
import zio.test.TestAspect._

object DocumentedSpec extends DefaultRunnableSpec with JsonSpec {
  def spec =
    suite("Documented Spec")(
      suite("JSON Codec")(
        testM("The JSON Codec should be well behaving") {
          val data     = Map("Dog" -> "Snoopy", "Cat" -> "Garfield", "Pig" -> "Babe")
          val original = Documented("This is documentation.", data)
          checkCodecIsWellBehaved(original)
        } //@@ debug
      )
    ) @@ silent
}
