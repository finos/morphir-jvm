package org.morphir.sdk

import zio.test._
import zio.test.Assertion._
import org.morphir.sdk.maybe.Maybe

object StringSpec extends DefaultRunnableSpec {
  def spec = suite("StringSpec")(
    suite("string.isEmpty specs")(
      isEmptyTests(
        "Hello World" -> false,
        ""            -> true
      ): _*
    ),
    suite("string.length specs")(
      lengthTests(
        "Hello World" -> 11,
        ""            -> 0
      ): _*
    ),
    suite("string.reverse specs")(
      reverseTests(
        "Hello World" -> "dlroW olleH",
        ""            -> ""
      ): _*
    ),
    suite("string.repeat specs")(
      repeatTests(
        (3, "ha", "hahaha")
      ): _*
    ),
    suite("string.replace specs")(
      replaceTests(
        (".", "-", "Json.Decode.succeed", "Json-Decode-succeed"),
        (",", "/", "a,b,c,d,e", "a/b/c/d/e")
      ): _*
    ),
    suite("string.fromInt specs")(
      fromIntTests(
        1  -> "1",
        -1 -> "-1"
      ): _*
    ),
    suite("string.append specs")(
      appendTests(
        ("butter", "fly", "butterfly")
      ): _*
    )
  )

  def isEmptyTests(cases: (string.String, Boolean)*) =
    cases.map {
      case (input, expected) =>
        test(
          s"Given a string: '$input' calling isEmpty should return '$expected'"
        ) {
          assert(string.isEmpty(input))(equalTo(expected))
        }
    }

  def lengthTests(cases: (string.String, Int)*) =
    cases.map {
      case (input, expected) =>
        test(
          s"Given a string: '$input' calling length should return '$expected'"
        ) {
          assert(string.length(input))(equalTo(expected))
        }
    }

  def reverseTests(cases: (string.String, string.String)*) =
    cases.map {
      case (input, expected) =>
        test(
          s"Given a string: '$input' calling reverse should return '$expected'"
        ) {
          assert(string.reverse(input))(equalTo(expected))
        }
    }

  def repeatTests(cases: (Int, string.String, string.String)*) =
    cases.map {
      case (inputInt, inputStr, expected) =>
        test(
          s"Given a string: '$inputStr' and an Int: '$inputInt' calling repeat should return '$expected'"
        ) {
          assert(string.repeat(inputInt, inputStr))(equalTo(expected))
        }
    }

  def replaceTests(cases: (string.String, string.String, string.String, string.String)*) =
    cases.map {
      case (literal, replacement, target, expected) =>
        test(
          s"Given a string: '$target' calling replace should return '$expected'"
        ) {
          assert(string.replace(literal, replacement, target))(equalTo(expected))
        }
    }

  def fromIntTests(cases: (Int, string.String)*) =
    cases.map {
      case (input, expected) =>
        test(
          s"Given an int: '$input' calling fromInt should return '$expected'"
        ) {
          assert(string.fromInt(input))(equalTo(expected))
        }
    }

  def appendTests(cases: (string.String, string.String, string.String)*) =
    cases.map {
      case (first, second, expected) =>
        test(
          s"Given Strings: '$first' and '$second' calling append should return '$expected'"
        ) {
          assert(string.append(first, second))(equalTo(expected))
        }
    }

  def concatTests(cases: (List[string.String], string.String)*) =
    cases.map {
      case (input, expected) =>
        test(
          s"Given a list[string]: '$input' calling concat should return '$expected'"
        ) {
          assert(string.concat(input))(equalTo(expected))
        }
    }

  def splitTests(cases: (string.String, string.String, List[String])*) =
    cases.map {
      case (sep, target, expected) =>
        test(
          s"Given Strings: '$target' and 'sep' calling split should return '$expected'"
        ) {
          assert(string.split(sep, target))(equalTo(expected))
        }
    }

  def toIntTests(cases: (string.String, Maybe[Int])*) =
    cases.map {
      case (input, expected) =>
        test(
          s"Given a string: '$input' calling toInt should return '$expected'"
        ) {
          assert(string.toInt(input))(equalTo(expected))
        }
    }

  def toUpperTests(cases: (string.String, string.String)*) =
    cases.map {
      case (input, expected) =>
        test(
          s"Given a string: '$input' calling toUpper should return '$expected'"
        ) {
          assert(string.toUpper(input))(equalTo(expected))
        }
    }

  def toLowerTests(cases: (string.String, string.String)*) =
    cases.map {
      case (input, expected) =>
        test(
          s"Given a string: '$input' calling toLower should return '$expected'"
        ) {
          assert(string.toLower(input))(equalTo(expected))
        }
    }

  def trimTests(cases: (string.String, string.String)*) =
    cases.map {
      case (input, expected) =>
        test(
          s"Given a string: '$input' calling trim should return '$expected'"
        ) {
          assert(string.trim(input))(equalTo(expected))
        }
    }

  def joinTests(cases: (Char, List[string.String], string.String)*) =
    cases.map {
      case (sep, chunks, expected) =>
        test(
          s"Given a list[string]: '$chunks' calling join should return '$expected'"
        ) {
          assert(string.join(sep)(chunks))(equalTo(expected))
        }
    }

  def wordsTests(cases: (string.String, List[String])*) =
    cases.map {
      case (input, expected) =>
        test(
          s"Given a string: '$input' calling words should return '$expected'"
        ) {
          assert(string.words(input))(equalTo(expected))
        }
    }

  def linesTests(cases: (string.String, List[String])*) =
    cases.map {
      case (input, expected) =>
        test(
          s"Given a string: '$input' calling lines should return '$expected'"
        ) {
          assert(string.lines(input))(equalTo(expected))
        }
    }

  def sliceTests(cases: (Int, Int, string.String, string.String)*) =
    cases.map {
      case (start, end, string, expected) =>
        test(
          s"Given a string: '$string' and Ints: $start and $end calling slice should return '$expected'"
        ) {
          assert(string.slice(start)(end)(string))(equalTo(expected))
        }
    }

  def leftTests(cases: (Int, string.String, string.String)*) =
    cases.map {
      case (n, str, expected) =>
        test(
          s"Given a string: '$str' calling left should return '$expected'"
        ) {
          assert(string.left(n)(str))(equalTo(expected))
        }
    }

  def rightTests(cases: (Int, string.String, string.String)*) =
    cases.map {
      case (n, str, expected) =>
        test(
          s"Given a string: '$str' calling right should return '$expected'"
        ) {
          assert(string.right(n)(str))(equalTo(expected))
        }
    }

  def dropLeftTests(cases: (Int, string.String, string.String)*) =
    cases.map {
      case (n, str, expected) =>
        test(
          s"Given a string: '$str' calling dropLeft should return '$expected'"
        ) {
          assert(string.dropLeft(n)(str))(equalTo(expected))
        }
    }

  def dropRightTests(cases: (Int, string.String, string.String)*) =
    cases.map {
      case (n, str, expected) =>
        test(
          s"Given a string: '$str' calling dropRight should return '$expected'"
        ) {
          assert(string.dropRight(n)(str))(equalTo(expected))
        }
    }

  def containsTests(cases: (string.String, string.String, Boolean)*) =
    cases.map {
      case (substring, str, expected) =>
        test(
          s"Given Strings: '$substring' and '$str' calling contains should return '$expected'"
        ) {
          assert(string.contains(substring)(str))(equalTo(expected))
        }
    }

  def startsWithTests(cases: (string.String, string.String, Boolean)*) =
    cases.map {
      case (substring, str, expected) =>
        test(
          s"Given Strings: '$substring' and '$str' calling startsWith should return '$expected'"
        ) {
          assert(string.startsWith(substring)(str))(equalTo(expected))
        }
    }

  def endsWithTests(cases: (string.String, string.String, Boolean)*) =
    cases.map {
      case (substring, str, expected) =>
        test(
          s"Given string: '$substring' and '$str' calling endsWith should return '$expected'"
        ) {
          assert(string.endsWith(substring)(str))(equalTo(expected))
        }
    }

  def indexesTests(cases: (string.String, string.String, List[Int])*) =
    cases.map {
      case (substring, str, expected) =>
        test(
          s"Given string: '$substring' and '$str' calling indexes should return '$expected'"
        ) {
          assert(string.indexes(substring)(str))(equalTo(expected))
        }
    }

  def toFloatTests(cases: (string.String, Maybe[Float])*) =
    cases.map {
      case (input, expected) =>
        test(
          s"Given a string: '$input' calling toFloat should return '$expected'"
        ) {
          assert(string.toFloat(input))(equalTo(expected))
        }
    }

  def fromFloatTests(cases: (Float, string.String)*) =
    cases.map {
      case (input, expected) =>
        test(
          s"Given a Float: '$input' calling fromFloat should return '$expected'"
        ) {
          assert(string.fromFloat(input))(equalTo(expected))
        }
    }

  def fromCharTests(cases: (Char, string.String)*) =
    cases.map {
      case (input, expected) =>
        test(
          s"Given a char: '$input' calling fromChar should return '$expected'"
        ) {
          assert(string.fromChar(input))(equalTo(expected))
        }
    }

  def consTests(cases: (Char, string.String, string.String)*) =
    cases.map {
      case (ch, str, expected) =>
        test(
          s"Given a string: '$str' calling cons should return '$expected'"
        ) {
          assert(string.cons(ch)(str))(equalTo(expected))
        }
    }

  def unconsTests(cases: (string.String, Maybe[(Char, String)])*) =
    cases.map {
      case (input, expected) =>
        test(
          s"Given a string: '$input' calling uncons should return '$expected'"
        ) {
          assert(string.uncons(input))(equalTo(expected))
        }
    }

  def toListTests(cases: (string.String, List[Char])*) =
    cases.map {
      case (input, expected) =>
        test(
          s"Given a string: '$input' calling toList should return '$expected'"
        ) {
          assert(string.toList(input))(equalTo(expected))
        }
    }

  def padTests(cases: (Int, Char, string.String, string.String)*) =
    cases.map {
      case (n, ch, str, expected) =>
        test(
          s"Given a string: '$str', a Char: '$ch', and Int: '$n' calling pad should return '$expected'"
        ) {
          assert(string.pad(n)(ch)(str))(equalTo(expected))
        }
    }

  def padLeftTests(cases: (Int, Char, string.String, string.String)*) =
    cases.map {
      case (n, ch, str, expected) =>
        test(
          s"Given a string: '$str', a Char: '$ch', and Int: '$n' calling pad should return '$expected'"
        ) {
          assert(string.padLeft(n)(ch)(str))(equalTo(expected))
        }
    }

  def padRightTests(cases: (Int, Char, string.String, string.String)*) =
    cases.map {
      case (n, ch, str, expected) =>
        test(
          s"Given a string: '$str', a Char: '$ch', and Int: '$n' calling pad should return '$expected'"
        ) {
          assert(string.padRight(n)(ch)(str))(equalTo(expected))
        }
    }

  def trimLeftTests(cases: (string.String, string.String)*) =
    cases.map {
      case (input, expected) =>
        test(
          s"Given a string: '$input' calling trimLeft should return '$expected'"
        ) {
          assert(string.trimLeft(input))(equalTo(expected))
        }
    }

  def trimRightTests(cases: (string.String, string.String)*) =
    cases.map {
      case (input, expected) =>
        test(
          s"Given a string: '$input' calling trimRight should return '$expected'"
        ) {
          assert(string.trimRight(input))(equalTo(expected))
        }
    }
}
