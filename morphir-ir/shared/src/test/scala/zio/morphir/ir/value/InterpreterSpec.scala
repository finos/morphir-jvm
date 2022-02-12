package zio.morphir.ir.value

import java.math.BigInteger
import zio.test.*
import zio.morphir.ir.Literal
import zio.morphir.ir.LiteralValue
import zio.morphir.ir.Name
import zio.{Chunk, ZEnvironment}
import zio.morphir.ir.ValueModule.{Value, ValueCase}
import zio.morphir.ir.NativeFunction
import zio.morphir.ir.testing.MorphirBaseSpec
import zio.morphir.syntax.ValueSyntax
import zio.morphir.Dsl
import ValueCase.PatternCase

object InterpreterSpec extends MorphirBaseSpec with ValueSyntax {
  def spec = suite("Interpreter")(
    suite("native functions")(
      suite("addition")(
        test("Should evaluate correctly") {
          assertTrue(
            Interpreter.evaluate(additionExample) == Right(new BigInteger("3"))
          )
        }
      ),
      suite("subtraction")(
        test("Should evaluate correctly") {
          assertTrue(Interpreter.evaluate(subtractionExample) == Right(new BigInteger("-1")))
        }
      )
    ),
    suite("tuple case")(
      test("Should evaluate correctly") {
        assertTrue(Interpreter.evaluate(tupleCaseExample) == Right((new BigInteger("1"), new BigInteger("2"))))
      }
    ),
    suite("list case")(
      test("Should evaluate correctly") {
        assertTrue(Interpreter.evaluate(listCaseExample) == Right(List("hello", "world")))
      }
    ),
    suite("if then else case")(
      test("Should evaluate correctly") {
        assertTrue(Interpreter.evaluate(ifThenElseCaseExample) == Right("no"))
      }
    ),
    suite("record case")(
      test("Should evaluate correctly") {
        assertTrue(
          Interpreter.evaluate(recordCaseExample) == Right(
            Map(
              Name.unsafeMake(List("field", "a")) -> "hello",
              Name.unsafeMake(List("field", "b")) -> new BigInteger("2")
            )
          )
        )
      }
    ),
    suite("let recursion case")(
      test("Multiple bindings that do not refer to each other") {
        assertTrue(Interpreter.evaluate(letIntroduceMultipleExample) == Right(new BigInteger("42")))
      }
    ),
    suite("apply case")(
      test("Apply field function") {
        assertTrue(Interpreter.evaluate(applyFieldFunction) == Right("hello"))
      },
      test("Apply lambda with wildcard") {
        assertTrue(Interpreter.evaluate(applyWithWildCard) == Right(new BigInteger("42")))
      }
    ),
    suite("pattern matching")(
      suite("literal")(),
      suite("wildcard")(
        test("Should evaluate correctly") {
          assertTrue(Interpreter.evaluate(patternMatchWildcardCaseExample) == Right(new BigInteger("100")))
        }
      ),
      suite("as")(
        test("Should evaluate correctly") {
          assertTrue(Interpreter.evaluate(patternMatchAsCasExample) == Right(new BigInteger("42")))
        }
      )
      // a @ b @ 1
      // a @List( b, 2)
      //
    )
  )
  // /x = if (foo) y else 0
  // y = if (!foo) x else 0
  val letIntroduceMultipleExample: Value[Any] = Value {
    ValueCase.LetRecursionCase(
      Map(
        Name.fromString("x") -> Value(ValueCase.LiteralCase(LiteralValue.WholeNumber(new BigInteger("20")))),
        Name.fromString("y") -> Value(ValueCase.LiteralCase(LiteralValue.WholeNumber(new BigInteger("22"))))
      ),
      Value(
        ValueCase.NativeApplyCase(
          NativeFunction.Addition,
          Chunk(Value(ValueCase.VariableCase(Name("x"))), Value(ValueCase.VariableCase(Name("y"))))
        )
      )
    )

  }

  val applyFieldFunction: Value[Any] =
    Dsl.apply(Value(ValueCase.FieldFunctionCase(Name.fromString("fieldA"))), recordCaseExample)

  val additionExample: Value[Any] =
    Value {
      ValueCase.LetDefinitionCase(
        Name("x"),
        Value(ValueCase.LiteralCase(LiteralValue.WholeNumber(new BigInteger("1")))),
        Value(
          ValueCase.LetDefinitionCase(
            Name("y"),
            Value(ValueCase.LiteralCase(LiteralValue.WholeNumber(new BigInteger("2")))),
            Value(
              ValueCase.NativeApplyCase(
                NativeFunction.Addition,
                Chunk(Value(ValueCase.VariableCase(Name("x"))), Value(ValueCase.VariableCase(Name("y"))))
              )
            )
          )
        )
      )
    }

  val subtractionExample: Value[Any] =
    Value {
      ValueCase.LetDefinitionCase(
        Name("x"),
        Value(ValueCase.LiteralCase(LiteralValue.WholeNumber(new java.math.BigInteger("1")))),
        Value(
          ValueCase.LetDefinitionCase(
            Name("y"),
            Value(ValueCase.LiteralCase(LiteralValue.WholeNumber(new java.math.BigInteger("2")))),
            Value(
              ValueCase.NativeApplyCase(
                NativeFunction.Subtraction,
                Chunk(Value(ValueCase.VariableCase(Name("x"))), Value(ValueCase.VariableCase(Name("y"))))
              )
            )
          )
        )
      )
    }

  val tupleCaseExample: Value[Any] =
    Value(
      ValueCase.TupleCase(
        Chunk(
          Value(ValueCase.LiteralCase(LiteralValue.WholeNumber(new java.math.BigInteger("1")))),
          Value(ValueCase.LiteralCase(LiteralValue.WholeNumber(new java.math.BigInteger("2"))))
        )
      )
    )

  val listCaseExample: Value[Any] =
    Value(
      ValueCase.ListCase(
        Chunk(
          Value(ValueCase.LiteralCase(LiteralValue.String("hello"))),
          Value(ValueCase.LiteralCase(LiteralValue.String("world")))
        )
      )
    )

  val ifThenElseCaseExample: Value[Any] =
    Value(
      ValueCase.IfThenElseCase(
        condition = Value(ValueCase.LiteralCase(Literal.boolean(false))),
        thenBranch = Value(ValueCase.LiteralCase(Literal.string("yes"))),
        elseBranch = Value(ValueCase.LiteralCase(Literal.string("no")))
      )
    )

  lazy val recordCaseExample = {
    val fieldA = Name.fromString("fieldA")
    val fieldB = Name.fromString("fieldB")

    val value1 = Dsl.string("hello")
    val value2 = Dsl.wholeNumber(new java.math.BigInteger("2"))

    val element1 = fieldA -> value1
    val element2 = fieldB -> value2
    Dsl.record(element1, element2)
  }

  val patternMatchWildcardCaseExample =
    Dsl.patternMatch(
      Dsl.wholeNumber(new java.math.BigInteger("42")),
      Value(PatternCase.WildcardPatternCase, ZEnvironment.empty) -> Dsl.wholeNumber(
        new java.math.BigInteger("100")
      )
    )

  val patternMatchAsCasExample =
    Dsl.patternMatch(
      Dsl.wholeNumber(new java.math.BigInteger("42")),
      Value(
        PatternCase.AsPatternCase(Value(PatternCase.WildcardPatternCase, ZEnvironment.empty), Name.fromString("x")),
        ZEnvironment.empty
      ) -> Dsl.variable(Name.fromString("x"))
    )

  val applyWithWildCard = Dsl.apply(Dsl.lambda(Dsl.wildcard, Dsl.wholeNumber(new java.math.BigInteger("42"))), Dsl.unit)

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
  //         Chunk(Value(ValueCase.VariableCase(Name("x"))), Value(ValueCase.VariableCase(Name("y"))))
  //       )
  //   )
}
