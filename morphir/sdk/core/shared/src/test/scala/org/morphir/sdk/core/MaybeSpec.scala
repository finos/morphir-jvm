package org.morphir.sdk.core

import zio.test._
import zio.test.Assertion._

object MaybeSpec extends DefaultRunnableSpec {
  def spec = suite("MaybeSpec")(
    suite("Just specs")(
      testM("Just should be convertible to Some") {
        check(Gen.alphaNumericChar) { input =>
          val maybe = Maybe.Just(input)
          assert(Maybe.toOption(maybe))(isSome(equalTo(input)))
        }
      },
      suite("Calling withDefault")(
        test("Should produce the default value when the value is 'Nothing'") {
          assert(Maybe.withDefault("DEFAULT")(Maybe.Nothing))(
            equalTo("DEFAULT")
          )
        },
        test("Should produce the original value if the original is 'Just'") {
          assert(Maybe.withDefault("DEFAULT")(Maybe.just("Fish Sticks")))(
            equalTo("Fish Sticks")
          )
        }
      ),
      suite("Calling map")(
        test("Should return a mapped value when the SUT is a 'Just'") {
          val sut = Maybe.just(42)
          assert(Maybe.map((v: Int) => v * 2)(sut))(
            equalTo(Maybe.just(84))
          )
        },
        test(
          "Should be capable of returning a mapped value of another type when the SUT is a 'Just'"
        ) {
          val sut = Maybe.just(42)
          assert(Maybe.map((_: Int) => "Forty Two")(sut))(
            equalTo(Maybe.just("Forty Two"))
          )
        },
        test(
          "Should be capable of returning a mapped value of a non-primitive type when the SUT is a 'Just'"
        ) {
          val sut = Maybe.just(42)
          assert(Maybe.map((_: Int) => Wrapped(42))(sut))(
            equalTo(Maybe.just(Wrapped(42)))
          )
        },
        test("Should return 'Nothing' value when the SUT is 'Nothing'") {
          val sut = Maybe.Nothing
          assert(Maybe.map((v: Int) => v * 2)(sut))(
            equalTo(Maybe.Nothing)
          )
        }
      ),
      suite("Calling andThen")(
        testM("Should return a mapped 'Just' value for an input that is 'Just'") {
          check(Gen.int(0, 255)) { input =>
            val sut = Maybe.just(input.toString())
            assert(Maybe.andThen((v: String) => Maybe.Just(v.toInt))(sut))(
              equalTo(Maybe.just(input))
            )
          }

        }
      )
    )
  )

  case class Wrapped[A](value: A)
}
