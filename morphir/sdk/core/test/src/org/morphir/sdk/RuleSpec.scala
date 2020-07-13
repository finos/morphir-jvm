package org.morphir.sdk

import zio.test.Assertion._
import zio.test.{ suite, _ }

object RuleSpec extends DefaultRunnableSpec {
  def spec = suite("RuleSpec")(
    suite("rule.chain specs")(
      chainTests(
        (List(), Char.from('a'), Maybe.Nothing),
        (List((_: Char.Char) => Maybe.Nothing), Char.from('a'), Maybe.Nothing),
        (
          List((_: Char.Char) => Maybe.Nothing, (a: Char.Char) => Maybe.Just(a)),
          Char.from('a'),
          Maybe.Just(Char.from('a'))
        ),
        (
          List((_: Char.Char) => Maybe.Just(Char.from('b')), (a: Char.Char) => Maybe.Just(a)),
          Char.from('a'),
          Maybe.Just(Char.from('b'))
        )
      ): _*
    ),
    suite("rule.any specs")(
      testM("Calling any on anything should return True") {
        check(Gen.alphaNumericChar)(input => assert(rule.any(input))(equalTo(Bool.True)))
      }
    ),
    suite("rule.is specs")(
      testM("Calling is by passing in the same value twice should return True") {
        check(Gen.alphaNumericChar)(input => assert(rule.is(input)(input))(equalTo(Bool.True)))
      },
      testM("Calling is by passing in two different values should return False") {
        val gen =
          for {
            ref   <- Gen.alphaNumericString
            input <- Gen.alphaNumericString
            if ref != input
          } yield (ref, input)
        check(gen) {
          case (ref, input) => assert(rule.is(ref)(input))(equalTo(Bool.False))
        }
      }
    ),
    suite("rule.anyOf specs")(
      testM("Calling anyOf by passing in a list and a member should return True") {
        val gen =
          for {
            ref <- Gen.listOf(Gen.alphaNumericString)
            if ref.nonEmpty
          } yield (ref, ref.head)
        check(gen) {
          case (ref, input) => assert(rule.anyOf(ref)(input))(equalTo(Bool.True))
        }
      },
      testM("Calling anyOf by passing in a list and a non-member should return False") {
        val gen =
          for {
            ref   <- Gen.listOf(Gen.alphaNumericString)
            input <- Gen.alphaNumericString
            if !ref.contains(input)
          } yield (ref, input)
        check(gen) {
          case (ref, input) => assert(rule.anyOf(ref)(input))(equalTo(Bool.False))
        }
      }
    ),
    suite("rule.noneOf specs")(
      testM("Calling noneOf by passing in a list and a member should return False") {
        val gen =
          for {
            ref <- Gen.listOf(Gen.alphaNumericString)
            if ref.nonEmpty
          } yield (ref, ref.head)
        check(gen) {
          case (ref, input) => assert(rule.noneOf(ref)(input))(equalTo(Bool.False))
        }
      },
      testM("Calling noneOf by passing in a list and a non-member should return True") {
        val gen =
          for {
            ref   <- Gen.listOf(Gen.alphaNumericString)
            input <- Gen.alphaNumericString
            if !ref.contains(input)
          } yield (ref, input)
        check(gen) {
          case (ref, input) => assert(rule.noneOf(ref)(input))(equalTo(Bool.True))
        }
      }
    )
  )

  def chainTests(cases: (List[rule.Rule[Char.Char, Char.Char]], Char.Char, Maybe.Maybe[Char.Char])*) =
    cases.map {
      case (rules, input, expectedResult) =>
        test(
          s"Given the rules: '$rules' passing in input: '$input' chain should return '$expectedResult'"
        ) {
          assert(rule.chain(rules)(input))(equalTo(expectedResult))
        }
    }

}
