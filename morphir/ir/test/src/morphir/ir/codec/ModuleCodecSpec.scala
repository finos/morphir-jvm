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


package morphir.ir.codec

import morphir.ir.path.Path
import morphir.ir.Name
import zio.test._
import zio.test.Assertion._
import morphir.ir.json.JsonFacade

object ModuleCodecSpec extends DefaultRunnableSpec with JsonFacade {
  def spec = suite("ModuleCodec Spec")(
    suite("ModulePath JSON")(
      suite("Encoding")(
        test("A ModulePath should encode to the same JSON as a regular path") {
          val path = Path.fromNames(
            Name.name("alpha", "omega"),
            Name.name("beta", "delta"),
            Name.name("gamma")
          )

          val modulePath = path.toModulePath

          assert(encodeAsJson(modulePath))(equalTo(encodeAsJson(path)))

        }
      ),
      suite("Decoding")()
    )
  )
}
