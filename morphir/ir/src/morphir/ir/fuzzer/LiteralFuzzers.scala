package morphir.ir.fuzzer

import morphir.ir.Literal
import morphir.ir.Literal._

import zio.test.Gen

trait LiteralFuzzers {
  implicit val fuzzBoolLiteral: Fuzzer[BoolLiteral] =
    Gen.boolean.map(Literal.bool(_))

  implicit val fuzzCharLiteral: Fuzzer[CharLiteral] =
    Gen.anyUnicodeChar.map(Literal.char(_))

  implicit val fuzzStringLiteral: Fuzzer[StringLiteral] =
    Gen.alphaNumericString.map(Literal.string(_))

  implicit val fuzzIntLiteral: Fuzzer[IntLiteral] =
    Gen.anyInt.map(Literal.int(_))

  implicit val fuzzFloatLiteral: Fuzzer[FloatLiteral] =
    Gen.anyFloat.map(Literal.float(_))

  implicit val fuzzLiteral: Fuzzer[Literal] =
    Gen.oneOf(fuzzBoolLiteral, fuzzCharLiteral, fuzzStringLiteral, fuzzIntLiteral, fuzzFloatLiteral)
}

object LiteralFuzzers extends LiteralFuzzers
