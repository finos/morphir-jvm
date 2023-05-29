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

import io.circe.{ Json, parser }
import morphir.ir.FQName.FQName
import morphir.ir.Module.ModuleName
import morphir.ir.Name.Name
import morphir.ir.Package.PackageName
import morphir.ir.fqname.Codec
import zio.test.Assertion._
import zio.test._

object FQNameSpec extends DefaultRunnableSpec {
  val packageName: PackageName = List(List("morphir"), List("core"))
  val moduleName: ModuleName   = List(List("morphir"), List("i", "r"))
  val localName: Name          = List("f", "q", "name")
  val fqName: FQName           = (packageName, moduleName, localName)

  def spec: Spec[_root_.zio.test.environment.TestEnvironment, TestFailure[Nothing], TestSuccess] =
    suite("FQNameSpec")(
      test("Encoding FQName") {
        val encodedFQName = Codec.encodeFQName(fqName)

        val expectedJson = parser.parse("""[[["morphir"],["core"]],[["morphir"],["i","r"]],["f","q","name"]]""")

        assert(encodedFQName)(equalTo(expectedJson.getOrElse(Json.Null)))
      },
      test("Decoding FQName") {
        val parsedFQName =
          parser.parse("""[[["morphir"],["core"]],[["morphir"],["i","r"]],["f","q","name"]]""").getOrElse(Json.Null)

        val decodedFQName = Codec.decodeFQName(parsedFQName.hcursor)

        assert(decodedFQName.getOrElse(Json.Null))(equalTo(fqName))
      }
    )
}
