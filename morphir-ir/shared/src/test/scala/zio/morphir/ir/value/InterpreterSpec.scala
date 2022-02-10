package zio.morphir.ir.value

import java.math.BigInteger
import zio.test.*
import zio.morphir.ir.MorphirIR
import zio.morphir.ir.recursive.PatternCase
import zio.morphir.ir.recursive.ValueCase
import zio.morphir.ir.Literal
import zio.morphir.ir.LiteralValue
import zio.morphir.ir.Name
import zio.{Chunk, ZEnvironment}
import zio.morphir.ir.MorphirIR.Value
import zio.morphir.ir.NativeFunction
import zio.morphir.ir.testing.MorphirBaseSpec

object InterpreterSpec extends MorphirBaseSpec {
  def spec = suite("Interpreter")(
    suite("native functions")(
      suite("addition")(
        test("Should evaluate correctly") {
          assertTrue(
            Interpreter.eval(additionExample) == Right(new BigInteger("3"))
          )
        }
      ),
      suite("subtraction")(
        test("Should evaluate correctly") {
          assertTrue(Interpreter.eval(subtractionExample) == Right(new BigInteger("-1")))
        }
      )
    ),
    suite("tuple case")(
      test("Should evaluate correctly") {
        assertTrue(Interpreter.eval(tupleCaseExample) == Right((new BigInteger("1"), new BigInteger("2"))))
      }
    ),
    suite("list case")(
      test("Should evaluate correctly") {
        assertTrue(Interpreter.eval(listCaseExample) == Right(List("hello", "world")))
      }
    ),
    suite("if then else case")(
      test("Should evaluate correctly") {
        assertTrue(Interpreter.eval(ifThenElseCaseExample) == Right("no"))
      }
    ),
    suite("record case")(
      test("Should evaluate correctly") {
        assertTrue(
          Interpreter.eval(recordCaseExample) == Right(
            Map(
              Name.unsafeMake(List("field", "a")) -> "hello",
              Name.unsafeMake(List("field", "b")) -> new BigInteger("2")
            )
          )
        )
      }
    ),
    suite("pattern matching")(
      suite("literal")(),
      suite("wildcard")(
        test("Should evaluate correctly") {
          assertTrue(Interpreter.eval(patternMatchWildcardCaseExample) == Right(new BigInteger("100")))
        }
      ),
      suite("as")(
        test("Should evaluate correctly") {
          assertTrue(Interpreter.eval(patternMatchAsCasExample) == Right(new BigInteger("42")))
        }
      )
      // a @ b @ 1
      // a @List( b, 2)
      //
    )
  )

  val additionExample: MorphirIR[Any] =
    MorphirIR {
      ValueCase.LetDefinitionCase(
        Name("x"),
        MorphirIR(ValueCase.LiteralCase(LiteralValue.WholeNumber(new BigInteger("1")))),
        MorphirIR(
          ValueCase.LetDefinitionCase(
            Name("y"),
            MorphirIR(ValueCase.LiteralCase(LiteralValue.WholeNumber(new BigInteger("2")))),
            MorphirIR(
              ValueCase.NativeApplyCase(
                NativeFunction.Addition,
                Chunk(MorphirIR(ValueCase.VariableCase(Name("x"))), MorphirIR(ValueCase.VariableCase(Name("y"))))
              )
            )
          )
        )
      )
    }

  val subtractionExample: MorphirIR[Any] =
    MorphirIR {
      ValueCase.LetDefinitionCase(
        Name("x"),
        MorphirIR(ValueCase.LiteralCase(LiteralValue.WholeNumber(new java.math.BigInteger("1")))),
        MorphirIR(
          ValueCase.LetDefinitionCase(
            Name("y"),
            MorphirIR(ValueCase.LiteralCase(LiteralValue.WholeNumber(new java.math.BigInteger("2")))),
            MorphirIR(
              ValueCase.NativeApplyCase(
                NativeFunction.Subtraction,
                Chunk(MorphirIR(ValueCase.VariableCase(Name("x"))), MorphirIR(ValueCase.VariableCase(Name("y"))))
              )
            )
          )
        )
      )
    }

  val tupleCaseExample: MorphirIR[Any] =
    MorphirIR(
      ValueCase.TupleCase(
        Chunk(
          MorphirIR(ValueCase.LiteralCase(LiteralValue.WholeNumber(new java.math.BigInteger("1")))),
          MorphirIR(ValueCase.LiteralCase(LiteralValue.WholeNumber(new java.math.BigInteger("2"))))
        )
      )
    )

  val listCaseExample: MorphirIR[Any] =
    MorphirIR(
      ValueCase.ListCase(
        Chunk(
          MorphirIR(ValueCase.LiteralCase(LiteralValue.String("hello"))),
          MorphirIR(ValueCase.LiteralCase(LiteralValue.String("world")))
        )
      )
    )

  val ifThenElseCaseExample: MorphirIR[Any] =
    MorphirIR(
      ValueCase.IfThenElseCase(
        condition = MorphirIR(ValueCase.LiteralCase(Literal.boolean(false))),
        thenBranch = MorphirIR(ValueCase.LiteralCase(Literal.string("yes"))),
        elseBranch = MorphirIR(ValueCase.LiteralCase(Literal.string("no")))
      )
    )

  val fieldA = Name.fromString("fieldA")
  val fieldB = Name.fromString("fieldB")

  val value1 = Value.string("hello")
  val value2 = Value.wholeNumber(new java.math.BigInteger("2"))

  val element1 = fieldA -> value1
  val element2 = fieldB -> value2

  val recordCaseExample = Value.record(element1, element2)

  val patternMatchWildcardCaseExample =
    Value.patternMatch(
      Value.wholeNumber(new java.math.BigInteger("42")),
      Value(PatternCase.WildcardCase, ZEnvironment.empty) -> Value.wholeNumber(new java.math.BigInteger("100"))
    )

  val patternMatchAsCasExample =
    Value.patternMatch(
      Value.wholeNumber(new java.math.BigInteger("42")),
      Value(
        PatternCase.AsCase(Value(PatternCase.WildcardCase, ZEnvironment.empty), Name.fromString("x")),
        ZEnvironment.empty
      ) -> Value.variable(Name.fromString("x"))
    )

  // val patternMatchAwfulExample =
  //   Value.patternMatch(
  //     Value.wholeNumber(new java.math.BigInteger("7")),
  //     Value(
  //       PatternCase.AsCase(
  //         PatternCase.AsCase(PatternCase.LiteralCase(Literal.wholeNumber(new BigInteger("7"))), Name("x")),
  //         Name("y")
  //       )
  //     ) ->
  //       ValueCase.NativeApplyCase(
  //         NativeFunction.Addition,
  //         Chunk(MorphirIR(ValueCase.VariableCase(Name("x"))), MorphirIR(ValueCase.VariableCase(Name("y"))))
  //       )
  //   )
}
