package zio.morphir.ir.value

import java.math.BigInteger
import zio.test.*
import zio.morphir.ir.Name
import zio.morphir.ir.testing.MorphirBaseSpec
import zio.morphir.ir.testing.CaseExample.*
import zio.morphir.ir.ValueModule.RawValue
import zio.morphir.IRModule.IR

object InterpreterSpec extends MorphirBaseSpec {

  val sampleIR = IR(
    valueSpecifications = Map.empty,
    valueDefinitions = Map.empty,
    typeSpecifications = Map(recordTypeName -> recordTypeAliasSpecification),
    typeConstructors = Map.empty
  )
  def evaluate(value: RawValue): Any = Interpreter.evaluate(value, sampleIR, Map.empty)

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
      },
      test("Should update correctly") {
        assertTrue(
          Interpreter.evaluate(recordCaseUpdateExample) == Right(
            Map(
              Name.unsafeMake(List("field", "a")) -> "hello",
              Name.unsafeMake(List("field", "b")) -> new BigInteger("3")
            )
          )
        )
      }
    ),
    suite("let recursion case")(
      test("Multiple bindings that do not refer to each other") {
        assertTrue(Interpreter.evaluate(letIntroduceMultipleExample) == Right(new BigInteger("42")))
      },
      test("Multiple bindings where earlier binding refers to later definition") {
        assertTrue(Interpreter.evaluate(letIntroduceOutOfOrderExample) == Right(new BigInteger("44")))
      },
      test("recursive let definition example") {
        assertTrue(Interpreter.evaluate(letRecExample) == Right(new BigInteger("6")))
      },
      test("Static scoping example") {
        assertTrue(Interpreter.evaluate(staticScopingExample) == Right("static"))
      }
    ),
    suite("let non recursion case")(
      test("Let destructor case") {
        assertTrue(Interpreter.evaluate(letDestructExample) == Right("red"))
      }
    ),
    suite("apply case")(
      test("Apply field function") {
        assertTrue(Interpreter.evaluate(applyFieldFunction) == Right("hello"))
      },
      test("Apply lambda with wildcard") {
        assertTrue(Interpreter.evaluate(applyWithWildCard) == Right(new BigInteger("42")))
      },
      test("Lambda defined in let") {
        assertTrue(Interpreter.evaluate(lambdaExample) == Right(new BigInteger("66")))
      }
    ),
    suite("constructor case") {
      test("Should evaluate correctly") {
        assertTrue(evaluate(constructorExample) == Right(("Adam", new BigInteger("42"))))
      }
    },
    suite("pattern matching")(
      suite("literal")(),
      suite("wildcard")(
        test("Should evaluate correctly") {
          assertTrue(Interpreter.evaluate(patternMatchWildcardCaseExample) == Right(new BigInteger("100")))
        }
      ),
      suite("as")(
        test("Should evaluate correctly") {
          assertTrue(Interpreter.evaluate(patternMatchAsCaseExample) == Right(new BigInteger("42")))
        }
      ),
      suite("as with literal")(
        test("Should evaluate correctly") {
          assertTrue(Interpreter.evaluate(patternMatchAsCaseComplexExample) == Right(new BigInteger("14")))
        }
      ),
      suite("tuple")(
        test("Should evaluate correctly") {
          assertTrue(Interpreter.evaluate(patternTupleCaseExample) == Right(new BigInteger("107")))
        }
      ),
      suite("singleton tuple")(
        test("Should evaluate correctly") {
          assertTrue(Interpreter.evaluate(patternTupleOneCaseExample) == Right("singleton tuple"))
        }
      ),
      suite("singleton non match tuple")(
        test("Should evaluate correctly") {
          assertTrue(Interpreter.evaluate(patternTupleOneCaseCounterExample) == Right("right"))
        }
      ),
      suite("head tail list")(
        test("Should evaluate correctly") {
          assertTrue(Interpreter.evaluate(patternHeadTailCaseExample) == Right(List("world")))
        }
      ),
      suite("empty list")(
        test("Should evaluate correctly") {
          assertTrue(Interpreter.evaluate(patternMatchEmptyListCaseExample) == Right("empty list"))
        }
      ),
      suite("unit")(
        test("Should evaluate correctly") {
          assertTrue(Interpreter.evaluate(patternUnitCaseExample) == Right("right"))
        }
      )
      // a @ b @ 1
      // a @List( b, 2)
      //
    )
  )

}
