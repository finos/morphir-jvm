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
import zio.test._
import zio.test.Assertion._
import zio.test.TestAspect._
import morphir.ir.fuzzer.NameFuzzers._
import morphir.ir.path.Path
import morphir.ir.testing.JsonSpec

object PathSpec extends DefaultRunnableSpec with JsonSpec {

  def spec =
    suite("PathSpec")(
      suite("Creating a Path from a String")(
        test("Should be possible when given a simple string") {
          assert(Path.fromString("Person"))(
            equalTo(Path(List(Name.fromString("person"))))
          )
        },
        test("Should be possible when given a dotted string") {
          assert(Path.fromString("blog.Author"))(
            equalTo(
              Path(
                List(Name.fromList(List("blog")), Name.fromList(List("author")))
              )
            )
          )
        }
      ),
      suite("Transforming a Path into a String")(
        test("Should be supported (a)") {
          val input =
            Path(
              Name("foo", "bar"),
              Name("baz")
            )

          assert(Path.toString(Name.toTitleCase, ".", input))(
            equalTo("FooBar.Baz")
          )
        },
        test("Should be supported (b)") {
          val input =
            Path(
              Name("foo", "bar"),
              Name("baz")
            )

          assert(Path.toString(Name.toSnakeCase, "/", input))(
            equalTo("foo_bar/baz")
          )
        }
      ),
      suite("Transforming to a list of Names")(
        test("Should be support via the toList function") {
          assert(
            Path.toList(Path(Name("Com", "Example"), Name("Hello", "World")))
          )(
            equalTo(List(Name("Com", "Example"), Name("Hello", "World")))
          )
        }
      ),
      suite("Checking if one Path is a prefix of another should:")(
        test("""Return true: Given path is "foo/bar" and prefix is "foo" """) {
          val sut    = Path.fromString("foo/bar")
          val prefix = Path.fromString("foo")

          assert(Path.isPrefixOf(prefix = prefix, path = sut))(isTrue)
        },
        test("""Return false: Given path is "foo/foo" and prefix is "bar" """) {
          val sut    = Path.fromString("foo/foo")
          val prefix = Path.fromString("bar")

          assert(Path.isPrefixOf(prefix = prefix, path = sut))(isFalse)
        },
        test("""Return true: Given equal paths""") {
          val sut    = Path.fromString("foo/bar/baz")
          val prefix = sut

          assert(Path.isPrefixOf(prefix = prefix, path = sut))(isTrue)
        }
      ),
      suite("Path: Encoding and Decoding")(
        testM(
          "Path when encoded and then decoded should be equivalent to the original"
        ) {
          checkM(Gen.listOfBounded(1, 3)(fuzzName)) { names =>
            val sut = Path.fromList(names)
            checkCodecIsWellBehaved(sut)
          }
        },
        checkEncodesTo(
          Path.fromNames(
            Name.name("alpha"),
            Name.name("beta"),
            Name.name("gamma")
          ),
          """[["alpha"],["beta"],["gamma"]]"""
        ),
        checkEncodesTo(
          Path.fromNames(
            Name.name("alpha", "omega"),
            Name.name("beta", "delta"),
            Name.name("gamma")
          ),
          """[["alpha","omega"],["beta","delta"],["gamma"]]"""
        )
      )
    ) @@ silent

  def checkEncodesTo(sut: Path, expectedJsonText: String): ZSpec[Any, Nothing] =
    test(s"Given Path: $sut it should encode to: $expectedJsonText") {
      assert(compactEncode(sut))(equalTo(expectedJsonText))
    }
}
