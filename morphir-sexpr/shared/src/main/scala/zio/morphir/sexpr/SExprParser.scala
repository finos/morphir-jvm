package zio.morphir.sexpr

import zio.Chunk
import zio.morphir.sexpr.ast.*
import zio.parser.*

object SExprParser {
  type SExprSyntax[-Value, +Result] = Syntax[String, Char, Char, Value, Result]

  object grammar {
    lazy val SPACE: Syntax[String, Char, Char, Unit, Unit] = Syntax.char(' ')
    lazy val COLON: Syntax[String, Char, Char, Unit, Unit] = Syntax.char(':')
    lazy val COMMA: Syntax[String, Char, Char, Unit, Unit] = Syntax.char(',')
    lazy val QUOTE: Syntax[String, Char, Char, Unit, Unit] = Syntax.char('\"')
    lazy val WS: Syntax[String, Char, Char, Char, Char]    = Syntax.charIn(' ', '\t', '\r', '\n', ',')
    lazy val WSS: Syntax[String, Char, Char, Unit, Unit]   = WS.*.asPrinted((), Chunk(' '))
    lazy val LF: Syntax[Unit, Char, Char, Unit, Unit]      = Syntax.char('\n', ())
    // lazy val comment = Syntax.char(';') ~> Syntax.anyChar.repeatUntil(LF.unit).unit

    lazy val symbolHead: Syntax[String, Char, Char, Char, Char] = Syntax.letter | Syntax.charIn('.', '/', '_', '#')
    lazy val symbolRest: Syntax[String, Char, Char, Char, Char] = symbolHead | Syntax.digit
    lazy val name: Syntax[
      String,
      Char,
      Char,
      (Char, Option[Chunk[Char]], Option[Chunk[Char]]),
      (Char, Option[Chunk[Char]], Option[Chunk[Char]])
    ] = symbolHead ~ symbolRest.repeat.? ~ (COLON ~> symbolRest.atLeast(1)).?
    lazy val nsSymbol: Syntax[
      String,
      Char,
      Char,
      (Char, Option[Chunk[Char]], Option[Chunk[Char]], (Char, Option[Chunk[Char]], Option[Chunk[Char]])),
      (Char, Option[Chunk[Char]], Option[Chunk[Char]], (Char, Option[Chunk[Char]], Option[Chunk[Char]]))
    ] = name ~ Syntax.char('/') ~ name
    lazy val symb: Syntax[
      String,
      Char,
      Char,
      (Char, Option[Chunk[Char]], Option[Chunk[Char]])
        with (Char, Option[Chunk[Char]], Option[Chunk[Char]], (Char, Option[Chunk[Char]], Option[Chunk[Char]])),
      Product with Serializable
    ] = name | nsSymbol
    lazy val keywordSimple: Syntax[
      String,
      Char,
      Char,
      (Char, Option[Chunk[Char]], Option[Chunk[Char]])
        with (Char, Option[Chunk[Char]], Option[Chunk[Char]], (Char, Option[Chunk[Char]], Option[Chunk[Char]])),
      Product with Serializable
    ] = COLON ~ symb
    lazy val keywordMacro: Syntax[
      String,
      Char,
      Char,
      (Char, Option[Chunk[Char]], Option[Chunk[Char]])
        with (Char, Option[Chunk[Char]], Option[Chunk[Char]], (Char, Option[Chunk[Char]], Option[Chunk[Char]])),
      Product with Serializable
    ] = COLON ~ keywordSimple
    lazy val combinedSymbol: Syntax[
      String,
      Char,
      Char,
      (Char, Option[Chunk[Char]], Option[Chunk[Char]])
        with (Char, Option[Chunk[Char]], Option[Chunk[Char]], (Char, Option[Chunk[Char]], Option[Chunk[Char]])),
      Product with Serializable
    ] = symb | keywordSimple | keywordMacro

    lazy val symbol: SExprSyntax[SExpr.Symbol, SExpr.Symbol] = combinedSymbol.string
      .transform(SExpr.Symbol.apply, (s: SExpr.Symbol) => s.value)

    lazy val nil: SExprSyntax[SExpr.Nil.type, SExpr.Nil.type] =
      Syntax.string("nil", SExpr.Nil)

    lazy val bool: SExprSyntax[SExpr.Bool, SExpr.Bool] =
      Syntax.string("true", SExpr.Bool.True) | Syntax.string("false", SExpr.Bool.False)

    lazy val escapedChar: SExprSyntax[Char, Char]      = Syntax.charNotIn('\"') // TODO: real escaping support
    lazy val quotedString: SExprSyntax[String, String] = (QUOTE ~> escapedChar.*.string <~ QUOTE)

    lazy val str: SExprSyntax[SExpr.Str, SExpr.Str] = quotedString
      .transform(SExpr.Str.apply, (s: SExpr.Str) => s.value)

    lazy val digits = Syntax.digit.repeat
    lazy val signedIntStr: SExprSyntax[(Option[Unit], Chunk[Char]), (Option[Unit], Chunk[Char])] =
      Syntax.char('-').? ~ digits
    lazy val frac: SExprSyntax[Chunk[Char], Chunk[Char]] = Syntax.char('.') ~> digits
    lazy val exp: SExprSyntax[(Char, Char, Chunk[Char]), (Char, Char, Chunk[Char])] =
      Syntax.charIn('e', 'E') ~ Syntax.charIn('+', '-') ~ digits
    lazy val number: SExprSyntax[String, String] = (signedIntStr ~ frac.? ~ exp.?).string

    lazy val num: SExprSyntax[SExpr.Num, SExpr.Num] =
      number.transform(n => SExpr.Num(BigDecimal(n)), (v: SExpr.Num) => v.value.toString())

    lazy val bigInt: SExprSyntax[String, BigInt] = signedIntStr.string.map(BigInt(_))

    lazy val vectorSep: SExprSyntax[Unit, Unit] = WSS
    lazy val vector: SExprSyntax[SExpr.SVector, SExpr.SVector] =
      (Syntax.char('[') ~> vectorSep ~> sexpr.repeatWithSep0(vectorSep) <~ vectorSep <~ Syntax.char(']'))
        .transform(SExpr.SVector.apply, (v: SExpr.SVector) => v.items)

    lazy val keyValueSep: SExprSyntax[Unit, Unit]                  = WSS
    lazy val keyValue: SExprSyntax[(SExpr, SExpr), (SExpr, SExpr)] = (sexpr ~ keyValueSep ~ sexpr)

    lazy val map: SExprSyntax[SExpr.SMap[SExpr, SExpr], SExpr.SMap[SExpr, SExpr]] =
      (Syntax.char('{') ~>
        vectorSep ~>
        keyValue.repeatWithSep0(vectorSep).surroundedBy(WSS) <~
        vectorSep <~
        Syntax.char('}'))
        .transform(
          (chunk: zio.Chunk[(SExpr, SExpr)]) => SExpr.SMap.apply(chunk.toMap),
          (map: SExpr.SMap[SExpr, SExpr]) => zio.Chunk.fromIterable[(SExpr, SExpr)](map.items)
        )

    lazy val sexpr: SExprSyntax[SExpr, SExpr] =
      nil.widen[SExpr] <>
        bool.widen[SExpr] <>
        symbol.widen[SExpr] <>
        str.widen[SExpr] <>
        num.widen[SExpr] <>
        vector.widen[SExpr] <>
        map.widen[SExpr]
  }
}
