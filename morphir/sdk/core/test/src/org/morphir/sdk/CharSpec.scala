package org.morphir.sdk

import zio.test._
import zio.test.Assertion._

object CharSpec extends DefaultRunnableSpec {
  def spec = suite("CharSpec")(
    suite("char.toCode specs")(
      toCodeTests(
        char.from('A')     -> 65,
        char.from('B')     -> 66,
        char.from(0x1F603) -> 0x1F603,
        char.from(120506)  -> 0x1D6BA
      ): _*
    ),
    suite("char.isUpper specs")(
      isUpperTests(
        char.from('A')     -> true,
        char.from('B')     -> true,
        char.from('Z')     -> true,
        char.from('0')     -> false,
        char.from('a')     -> false,
        char.from('-')     -> false,
        char.from(0x1D6BA) -> false
      ): _*
    ),
    suite("char.isLower specs")(
      isLowerTests(
        char.from('a') -> true,
        char.from('b') -> true,
        char.from('z') -> true,
        char.from('0') -> false,
        char.from('A') -> false,
        char.from('-') -> false,
        // Testing π - which is an uppercase unicode character
        char.from(0x1D6B7) -> false
      ): _*
    ),
    suite("char.isAlpha specs")(
      isAlphaTests(
        char.from('a') -> true,
        char.from('b') -> true,
        char.from('E') -> true,
        char.from('Y') -> true,
        char.from('0') -> false,
        char.from('-') -> false,
        // Testing π - which is an uppercase unicode character
        char.from(0x1D6B7) -> false
      ): _*
    ),
    suite("char.isAlphaNum specs")(
      isAlphaNumTests(
        char.from('a') -> true,
        char.from('b') -> true,
        char.from('E') -> true,
        char.from('Y') -> true,
        char.from('0') -> true,
        char.from('7') -> true,
        char.from('9') -> true,
        char.from('-') -> false,
        // Testing π - which is an uppercase unicode character
        char.from(0x1D6B7) -> false,
        char.from(0x1D6BA) -> false
      ): _*
    ),
    suite("char.isDigit specs")(
      isDigitTests(
        char.from('0') -> true,
        char.from('1') -> true,
        char.from('9') -> true,
        char.from('a') -> false,
        char.from('b') -> false,
        char.from('A') -> false,
        // Testing π - which is an uppercase unicode character
        char.from(0x1D6B7) -> false
      ): _*
    ),
    suite("char.isOctDigit specs")(
      isOctDigitTests(
        char.from('0') -> true,
        char.from('1') -> true,
        char.from('7') -> true,
        char.from('8') -> false,
        char.from('a') -> false,
        char.from('A') -> false,
        // Testing π - which is an uppercase unicode character
        char.from(0x1D6B7) -> false
      ): _*
    ),
    suite("char.isHexDigit specs")(
      testM(
        s"Given a character calliing isHexDigit should return true only for hex digits"
      ) {
        check(Gen.anyUnicodeChar) { input =>
          val sut      = char.from(input)
          val expected = "0123456789abcdefABCDEF".contains(input)
          assert(char.isHexDigit(sut))(equalTo(expected))
        }

      }
    )
  )

  def toCodeTests(cases: (char.Char, Int)*) =
    cases.map {
      case (input, expectedCode) =>
        test(
          s"Given a char: '$input' calliing toCode should return '$expectedCode'"
        ) {
          assert(char.toCode(input))(equalTo(expectedCode))
        }
    }

  def isUpperTests(cases: (char.Char, Boolean)*) =
    cases.map {
      case (input, expected) =>
        test(
          s"Given a char: '$input' calliing isUpper should return '$expected'"
        ) {
          assert(char.isUpper(input))(equalTo(expected))
        }
    }

  def isLowerTests(cases: (char.Char, Boolean)*) =
    cases.map {
      case (input, expected) =>
        test(
          s"Given a char: '$input' calliing isLower should return '$expected'"
        ) {
          assert(char.isLower(input))(equalTo(expected))
        }
    }

  def isAlphaTests(cases: (char.Char, Boolean)*) =
    cases.map {
      case (input, expected) =>
        test(
          s"Given a char: '$input' calliing isAlpha should return '$expected'"
        ) {
          assert(char.isAlpha(input))(equalTo(expected))
        }
    }

  def isAlphaNumTests(cases: (char.Char, Boolean)*) =
    cases.map {
      case (input, expected) =>
        test(
          s"Given a char: '$input' calliing isAlphaNum should return '$expected'"
        ) {
          assert(char.isAlphaNum(input))(equalTo(expected))
        }
    }

  def isDigitTests(cases: (char.Char, Boolean)*) =
    cases.map {
      case (input, expected) =>
        test(
          s"Given a char: '$input' calliing isDigit should return '$expected'"
        ) {
          assert(char.isDigit(input))(equalTo(expected))
        }
    }

  def isOctDigitTests(cases: (char.Char, Boolean)*) =
    cases.map {
      case (input, expected) =>
        test(
          s"Given a char: '$input' calliing isOctDigit should return '$expected'"
        ) {
          assert(char.isOctDigit(input))(equalTo(expected))
        }
    }
}
