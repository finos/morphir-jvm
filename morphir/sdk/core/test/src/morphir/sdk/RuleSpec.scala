package morphir.sdk

import zio.test.Assertion._
import zio.test.{ suite, _ }

object RuleSpec extends DefaultRunnableSpec {
  def spec = suite("RuleSpec")(
    suite("Rule.chain specs")(
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
    suite("Rule.any specs")(
      testM("Calling any on anything should return True") {
        check(Gen.alphaNumericChar)(input => assert(Rule.any(input))(equalTo(Bool.True)))
      }
    ),
    suite("Rule.is specs")(
      testM("Calling is by passing in the same value twice should return True") {
        check(Gen.alphaNumericChar)(input => assert(Rule.is(input)(input))(equalTo(Bool.True)))
      },
      testM("Calling is by passing in two different values should return False") {
        val gen =
          for {
            ref   <- Gen.alphaNumericString
            input <- Gen.alphaNumericString
            if ref != input
          } yield (ref, input)
        check(gen) {
          case (ref, input) => assert(Rule.is(ref)(input))(equalTo(Bool.False))
        }
      }
    ),
    suite("Rule.anyOf specs")(
      testM("Calling anyOf by passing in a list and a member should return True") {
        val gen =
          for {
            ref <- Gen.listOf(Gen.alphaNumericString)
            if ref.nonEmpty
          } yield (ref, ref.head)
        check(gen) {
          case (ref, input) => assert(Rule.anyOf(ref)(input))(equalTo(Bool.True))
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
          case (ref, input) => assert(Rule.anyOf(ref)(input))(equalTo(Bool.False))
        }
      }
    ),
    suite("Rule.noneOf specs")(
      testM("Calling noneOf by passing in a list and a member should return False") {
        val gen =
          for {
            ref <- Gen.listOf(Gen.alphaNumericString)
            if ref.nonEmpty
          } yield (ref, ref.head)
        check(gen) {
          case (ref, input) => assert(Rule.noneOf(ref)(input))(equalTo(Bool.False))
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
          case (ref, input) => assert(Rule.noneOf(ref)(input))(equalTo(Bool.True))
        }
      }
    )
  )

  def chainTests(cases: (List[Rule.Rule[Char.Char, Char.Char]], Char.Char, Maybe.Maybe[Char.Char])*) =
    cases.map {
      case (rules, input, expectedResult) =>
        test(
          s"Given the rules: '$rules' passing in input: '$input' chain should return '$expectedResult'"
        ) {
          assert(Rule.chain(rules)(input))(equalTo(expectedResult))
        }
    }

}
