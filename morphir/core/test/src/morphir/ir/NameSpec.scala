package morphir.ir

import zio.test._
import zio.test.Assertion._
import zio.test.environment._

import upickle.default._

object NameSpec extends DefaultRunnableSpec {
  def spec =
    suite("NameSpec")(
      suite("Make a Name from a string and check that:")(
        suite("Name should be creatable from a single word that:")(
          test("Starts with a capital letter") {
            assert(Name.fromString("Marco"))(
              equalTo(Name(List("marco")))
            )
          },
          test("Is all lowercase") {
            assert(Name.fromString("polo"))(equalTo(Name(List("polo"))))
          }
        ),
        suite("Name should be creatable from compound words that:")(
          test("Are formed from a snake case word") {
            assert(Name.fromString("super_mario_world"))(
              equalTo(Name(List("super", "mario", "world")))
            )
          },
          test("Contain many kinds of word delimiters") {
            assert(Name.fromString("fooBar_baz 123"))(
              equalTo(Name(List("foo", "bar", "baz", "123")))
            )
          },
          test("Are formed from a camel-cased string") {
            assert(Name.fromString("valueInUSD"))(
              equalTo(Name(List("value", "in", "u", "s", "d")))
            )
          },
          test("Are formed from a title-cased string") {

            assert(Name.fromString("ValueInUSD"))(
              equalTo(Name(List("value", "in", "u", "s", "d")))
            )
          },
          test("Are have a number in the middle") {

            assert(Name.fromString("Nintendo64VideoGameSystem"))(
              equalTo(Name(List("nintendo", "64", "video", "game", "system")))
            )
          },
          test("Complete and utter nonsense") {
            assert(Name.fromString("_-%"))(equalTo(Name(List.empty)))
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
        testM("Encoding and decoding a name should yield an equivalent value") {
          check(Name.fuzzName) { givenName =>
            assert(read[Name](writeJs(givenName)))(
              equalTo(givenName)
            )

          }
        },
        testM("A Name should encode as expected") {
          check(Name.fuzzName) { givenName =>
            assert(write(givenName))(equalTo(write(givenName.value)))
          }
        },
        test(
          """Given a Name whose value is ["delta","sigma","theta"] it should encode correctly"""
        ) {
          assert(write(Name.fromStrings("delta", "sigma", "theta")))(
            equalTo("""["delta","sigma","theta"]""")
          )
        },
        test(
          """Given a Name whose value is ["sigma","gamma","ro"] it should encode correctly"""
        ) {
          assert(write(Name.fromStrings("sigma", "gamma", "ro")))(
            equalTo("""["sigma","gamma","ro"]""")
          )
        }
      )
    )
}
