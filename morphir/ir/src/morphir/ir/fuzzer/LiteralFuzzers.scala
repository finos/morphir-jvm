package morphir.ir.fuzzer

import morphir.ir.literal
import morphir.ir.literal._
import morphir.ir.literal.Literal._

import zio.test.Gen

trait LiteralFuzzers {
  implicit val fuzzBoolLiteral: Fuzzer[BoolLiteral] =
    Gen.boolean.map(literal.bool)

  implicit val fuzzCharLiteral: Fuzzer[CharLiteral] =
    Gen.anyUnicodeChar.map(literal.char)

  implicit val fuzzStringLiteral: Fuzzer[StringLiteral] =
    Gen.alphaNumericString.map(literal.string)

  implicit val fuzzIntLiteral: Fuzzer[IntLiteral] =
    Gen.anyInt.map(literal.int)

  implicit val fuzzFloatLiteral: Fuzzer[FloatLiteral] =
    Gen.anyFloat.map(literal.float)

  implicit val fuzzLiteral: Fuzzer[Literal] =
    Gen.oneOf(fuzzBoolLiteral, fuzzCharLiteral, fuzzStringLiteral, fuzzIntLiteral, fuzzFloatLiteral)
}

object LiteralFuzzers extends LiteralFuzzers
