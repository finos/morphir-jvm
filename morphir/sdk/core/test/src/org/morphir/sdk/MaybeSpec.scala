package org.morphir.sdk

import maybe.Maybe
import zio.test._
import zio.test.Assertion._

object MaybeSpec extends DefaultRunnableSpec {
  def spec = suite("MaybeSpec")(
    suite("Just specs")(
      testM("Just should be convertible to Some") {
        check(Gen.alphaNumericChar) { input =>
          val maybe = maybe.Just(input)
          assert(maybe.toOption(maybe))(isSome(equalTo(input)))
        }
      },
      suite("Calling withDefault")(
        test("Should produce the default value when the value is 'Nothing'") {
          assert(maybe.withDefault("DEFAULT")(maybe.Nothing))(
            equalTo("DEFAULT")
          )
        },
        test("Should produce the original value if the original is 'Just'") {
          assert(maybe.withDefault("DEFAULT")(maybe.just("Fish Sticks")))(
            equalTo("Fish Sticks")
          )
        }
      ),
      suite("Calling map")(
        test("Should return a mapped value when the SUT is a 'Just'") {
          val sut = maybe.just(42)
          assert(maybe.map((v: Int) => v * 2)(sut))(
            equalTo(maybe.just(84))
          )
        },
        test(
          "Should be capable of returning a mapped value of another type when the SUT is a 'Just'"
        ) {
          val sut = maybe.just(42)
          assert(maybe.map((_: Int) => "Forty Two")(sut))(
            equalTo(maybe.just("Forty Two"))
          )
        },
        test(
          "Should be capable of returning a mapped value of a non-primitive type when the SUT is a 'Just'"
        ) {
          val sut = maybe.just(42)
          assert(maybe.map((_: Int) => Wrapped(42))(sut))(
            equalTo(maybe.just(Wrapped(42)))
          )
        },
        test("Should return 'Nothing' value when the SUT is 'Nothing'") {
          val sut = maybe.Nothing
          assert(maybe.map((v: Int) => v * 2)(sut))(
            equalTo(maybe.Nothing)
          )
        }
      ),
      suite("Calling andThen")(
        testM("Should return a mapped 'Just' value for an input that is 'Just'") {
          check(Gen.int(0, 255)) { input =>
            val sut = maybe.just(input.toString())
            assert(maybe.andThen((v: String) => maybe.Just(v.toInt))(sut))(
              equalTo(maybe.just(input))
            )
          }

        }
      ),
      suite("Foreach spec")(
        test("Given a Just foreach should execute the given function") {
          var entries              = list.empty[String]
          def push(entry: String)  = entries = entry :: entries
          val maybe: Maybe[String] = maybe.just("Hello")
          maybe.foreach(push)
          assert(entries)(equalTo(list("Hello")))
        },
        test("Given a Nothing foreach should NOT execute the given function") {
          var entries              = list.empty[String]
          def push(entry: String)  = entries = entry :: entries
          val maybe: Maybe[String] = maybe.Nothing
          maybe.foreach(push)
          assert(entries)(equalTo(list.empty))
        }
      ),
      suite("For comprehension spec")(
        test("Basic for loop should be supported") {
          var entries              = list.empty[String]
          def push(entry: String)  = entries = entry :: entries
          val maybe: Maybe[String] = maybe.just("Hello")
          for {
            m <- maybe
          } push(m)

          assert(entries)(equalTo(list("Hello")))
        },
        test("yield should be supported") {
          val result = for {
            a <- maybe.Just(42)
          } yield 2 * a
          assert(result)(equalTo(maybe.Just(84)))
        },
        testM("Multiple generators should be supported") {
          check(Gen.alphaNumericString, Gen.alphaNumericString) { (part1, part2) =>
            val result = for {
              a <- maybe.Just(part1)
              b <- maybe.Just(part2)
            } yield s"$a-$b"

            assert(result)(equalTo(maybe.just(s"$part1-$part2")))
          }
        },
        test("if expressions shoud be supported") {
          val result = for {
            a <- maybe.just(42)
            if (a % 2 == 0)
            b <- maybe.just(8)
          } yield a + b

          assert(result)(equalTo(maybe.just(50)))
        }
      )
    )
  )

  case class Wrapped[A](value: A)
}
