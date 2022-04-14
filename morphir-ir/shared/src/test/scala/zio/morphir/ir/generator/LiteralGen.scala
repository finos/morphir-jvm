package zio.morphir.ir.generator

import zio.morphir.ir.Literal
import zio.test.Gen

trait LiteralGen extends WordGen {
  final def asciiCharLiteral: Gen[Any, Literal.Char] = Gen.asciiChar.map(Literal.Char(_))

  final def boolLiteral: Gen[Any, Literal.Bool] = Gen.boolean.map(Literal.Bool(_))

  final def charLiteral: Gen[Any, Literal.Char]                       = Gen.char.map(Literal.Char(_))
  final def charLiteral(min: Char, max: Char): Gen[Any, Literal.Char] = Gen.char(min, max).map(Literal.Char(_))

  final def decimalLiteral(min: BigDecimal, max: BigDecimal): Gen[Any, Literal.Float] =
    Gen.bigDecimal(min, max).map(n => Literal.Float(n.bigDecimal))

  final def floatLiteral: Gen[Any, Literal.Float] = Gen.double.map(n => Literal.Float(java.math.BigDecimal.valueOf(n)))
  final def floatLiteral(min: BigDecimal, max: BigDecimal): Gen[Any, Literal.Float] =
    Gen.bigDecimal(min, max).map(n => Literal.Float(n.bigDecimal))

  final def literal: Gen[Any, Literal[Any]] = Gen.oneOf(
    boolLiteral,
    charLiteral,
    stringLiteral,
    floatLiteral,
    wholeNumberLiteral
  )

  def stringLiteral: Gen[Any, Literal.String] = words.map(Literal.String(_))

  def wholeNumberLiteral(min: BigInt, max: BigInt): Gen[Any, Literal.WholeNumber] =
    Gen.bigInt(min, max).map(n => Literal.WholeNumber(n.bigInteger))
  def wholeNumberLiteral(min: Long, max: Long): Gen[Any, Literal.WholeNumber] =
    Gen.long(min, max).map(n => Literal.WholeNumber(java.math.BigInteger.valueOf(n)))
  def wholeNumberLiteral: Gen[Any, Literal.WholeNumber] =
    Gen.long.map(n => Literal.WholeNumber(java.math.BigInteger.valueOf(n)))
}

object LiteralGen extends LiteralGen
