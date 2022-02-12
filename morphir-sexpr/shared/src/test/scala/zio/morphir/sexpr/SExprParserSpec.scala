package zio.morphir.sexpr

import zio.Chunk
import zio.test.DefaultRunnableSpec
import zio.test.*
import zio.morphir.sexpr.SExprParser.*
import zio.morphir.sexpr.SExprParser.grammar.*
import zio.morphir.sexpr.ast.SExpr
import zio.morphir.sexpr.ast.SymbolKind

object SExprParserSpec extends DefaultRunnableSpec {
  def spec = suite("SExpr Parser Spec")(
    test("Bool") {
      assertTrue(
        sexpr.parseString("true") == Right(SExpr.Bool.True),
        sexpr.parseString("false") == Right(SExpr.Bool.False)
      )
    },
    test("Nil") {
      assertTrue(
        sexpr.parseString("nil") == Right(SExpr.Nil)
      )
    },
    test("Str") {
      assertTrue(
        sexpr.parseString("\"nil\"") == Right(SExpr.Str("nil")),
        sexpr.parseString("\"null\"") == Right(SExpr.Str("null")),
        sexpr.parseString("\"\t\n\"") == Right(SExpr.Str("\t\n"))
      )
    },
    test("Num") {
      assertTrue(
        sexpr.parseString("22.22") == Right(SExpr.Num(BigDecimal("22.22"))),
        sexpr.parseString("2222") == Right(SExpr.Num(BigDecimal("2222"))),
        sexpr.parseString("-2222") == Right(SExpr.Num(BigDecimal("-2222"))),
        sexpr.parseString("-22.22") == Right(SExpr.Num(BigDecimal("-22.22")))
      )
    },
    test("Double") {
      check(Gen.double) { x =>
        assertTrue(
          grammar.num.parseString(x.toString) == Right(SExpr.Num(BigDecimal(x)))
        )
      }
    },
    test("Int") {
      check(Gen.int) { x =>
        assertTrue(
          grammar.num.parseString(x.toString) == Right(SExpr.Num(BigDecimal(x)))
        )
      }
    },
    test("Long") {
      check(Gen.long) { x =>
        assertTrue(
          grammar.num.parseString(x.toString) == Right(SExpr.Num(BigDecimal(x)))
        )
      }
    },
    test("Short") {
      check(Gen.short) { x =>
        assertTrue(
          grammar.num.parseString(x.toString) == Right(SExpr.Num(BigDecimal(x.toInt)))
        )
      }
    },
    test("Symbol") {
      assertTrue(
        sexpr.parseString("symb1") == Right(SExpr.Symbol("symb1", SymbolKind.Standard)),
        sexpr.parseString("null") == Right(SExpr.Symbol("null", SymbolKind.Standard)),
        sexpr.parseString("#symb") == Right(SExpr.Symbol("#symb", SymbolKind.Standard)),
        sexpr.parseString("_ns/il") == Right(SExpr.Symbol("_ns/il", SymbolKind.Standard)),
        sexpr.parseString(".n/il") == Right(SExpr.Symbol(".n/il", SymbolKind.Standard)),
        sexpr.parseString(":keyWord6") == Right(SExpr.Symbol(":keyWord6", SymbolKind.Keyword)),
        sexpr.parseString("::keywrdMacro") == Right(SExpr.Symbol("::keywrdMacro", SymbolKind.Macro))
      )
    },
    test("SVector") {
      assertTrue(
        sexpr.parseString("""[true -1 nil "Hello" null 3.14159 false]""") == Right(
          SExpr.SVector(
            Chunk(
              SExpr.Bool.True,
              SExpr.Num(BigDecimal(-1)),
              SExpr.Nil,
              SExpr.Str("Hello"),
              SExpr.Symbol("null"),
              SExpr.Num(BigDecimal(3.14159)),
              SExpr.Bool.False
            )
          )
        )
      )
    },
    test("SMap") {
      assertTrue(
        sexpr.parseString(
          """{ "x" 0, :keyword1 "hello", ::#vect [1 2.1 -0.3333], nil true }"""
        ) == Right(
          SExpr.SMap(
            Chunk(
              SExpr.Str("x")                                -> SExpr.Num(BigDecimal(0)),
              SExpr.Symbol(":keyword1", SymbolKind.Keyword) -> SExpr.Str("hello"),
              SExpr.Symbol("::#vect", SymbolKind.Macro) -> SExpr.SVector(
                Chunk(
                  SExpr.Num(BigDecimal(1)),
                  SExpr.Num(BigDecimal(2.1)),
                  SExpr.Num(BigDecimal(-0.3333))
                )
              ),
              SExpr.Nil -> SExpr.Bool.True
            ).toMap
          )
        )
      )
    }
  )
}
