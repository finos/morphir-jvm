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
import morphir.ir.testing.JsonSpec

object NameSpec extends DefaultRunnableSpec with JsonSpec {
  def spec =
    suite("NameSpec")(
      suite("Make a Name from a string and check that:")(
        suite("Name should be creatable from a single word that:")(
          test("Starts with a capital letter") {
            assert(Name.fromString("Marco"))(
              equalTo(Name("marco"))
            )
          },
          test("Is all lowercase") {
            assert(Name.fromString("polo"))(equalTo(Name("polo")))
          }
        ),
        suite("Name should be creatable from compound words that:")(
          test("Are formed from a snake case word") {
            assert(Name.fromString("super_mario_world"))(
              equalTo(Name("super", "mario", "world"))
            )
          },
          test("Contain many kinds of word delimiters") {
            assert(Name.fromString("fooBar_baz 123"))(
              equalTo(Name("foo", "bar", "baz", "123"))
            )
          },
          test("Are formed from a camel-cased string") {
            assert(Name.fromString("valueInUSD"))(
              equalTo(Name("value", "in", "u", "s", "d"))
            )
          },
          test("Are formed from a title-cased string") {

            assert(Name.fromString("ValueInUSD"))(
              equalTo(Name("value", "in", "u", "s", "d"))
            )
          },
          test("Are formed from a title-cased string (2)") {

            assert(Name("ValueInUSD"))(
              equalTo(Name("value", "in", "u", "s", "d"))
            )
          },
          test("Are have a number in the middle") {

            assert(Name.fromString("Nintendo64VideoGameSystem"))(
              equalTo(Name("nintendo", "64", "video", "game", "system"))
            )
          },
          test("Complete and utter nonsense") {
            assert(Name.fromString("_-%"))(equalTo(Name("")))
          }
        )
      ),
      suite("Name should be convertible to a title-case string:")(
        test(
          "When the name was originally constructed from a snake-case string"
        ) {
          val sut = Name.fromString("snake_case_input")
          assert(Name.toTitleCase(sut))(equalTo("SnakeCaseInput"))
        },
        test(
          "When the name was originally constructed from a camel-case string"
        ) {
          val sut = Name.fromString("camelCaseInput")
          assert(Name.toTitleCase(sut))(equalTo("CamelCaseInput"))
        }
      ),
      suite("Name should be convertible to a camel-case string:")(
        test(
          "When the name was originally constructed from a snake-case string"
        ) {
          val sut = Name.fromString("snake_case_input")
          assert(Name.toCamelCase(sut))(equalTo("snakeCaseInput"))
        },
        test(
          "When the name was originally constructed from a camel-case string"
        ) {
          val sut = Name.fromString("camelCaseInput")
          assert(Name.toCamelCase(sut))(equalTo("camelCaseInput"))
        }
      ),
      suite("Name should be convertible to snake-case")(
        test("When given a name constructed from a list of words") {
          val input = Name.fromList(List("foo", "bar", "baz", "123"))
          assert(Name.toSnakeCase(input))(equalTo("foo_bar_baz_123"))
        },
        test("When the name has parts of an abbreviation") {
          val name = Name.fromList(List("value", "in", "u", "s", "d"))
          assert(Name.toSnakeCase(name))(
            equalTo("value_in_USD")
          )
        }
      ),
      suite("Name should be convertible to kebab-case")(
        test("When given a name constructed from a list of words") {
          val input = Name.fromList(List("foo", "bar", "baz", "123"))
          assert(Name.toKebabCase(input))(equalTo("foo-bar-baz-123"))
        },
        test("When the name has parts of an abbreviation") {
          val name = Name.fromList(List("value", "in", "u", "s", "d"))
          assert(Name.toKebabCase(name))(
            equalTo("value-in-USD")
          )
        }
      ),
      suite("Name toHumanWords should provide a list of words from a Name")(
        test("When the name is from a camelCase string") {
          val sut = Name.fromString("ValueInUSD")
          assert(Name.toHumanWords(sut))(equalTo(List("value", "in", "USD")))
        }
      ),
      suite("Name encoding/decoding")(
        testM("Should work in a well-behaved manner") {
          checkM(fuzzName)(givenName => checkCodecIsWellBehaved(givenName))
        },
        testM("A Name should encode as expected") {
          check(fuzzName)(givenName => assertEncodesToExpectedCompactJsonString(givenName)(givenName.show))
        },
        test(
          """Given a Name whose value is ["delta","sigma","theta"] it should encode correctly"""
        ) {
          checkEncodesTo(name("delta", "sigma", "theta"))(
            ujson.Arr(
              ujson.Str("delta"),
              ujson.Str("sigma"),
              ujson.Str("theta")
            )
          )
        },
        test(
          """Given a Name whose value is ["sigma","gamma","ro"] it should encode correctly"""
        ) {
          checkEncodesTo(name("sigma", "gamma", "ro"))(
            ujson.Arr(
              ujson.Str("sigma"),
              ujson.Str("gamma"),
              ujson.Str("ro")
            )
          )
        }
      )
    ) @@ silent
}
