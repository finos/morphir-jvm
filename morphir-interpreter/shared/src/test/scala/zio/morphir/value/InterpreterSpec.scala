package zio.morphir.value

import java.math.BigInteger
import zio.test._
import zio.morphir.ir.Name
import zio.morphir.ir.testing.CaseExample._
import zio.morphir.ir.ValueModule.RawValue
import zio.morphir.IR
import zio.morphir.testing.MorphirBaseSpec

object InterpreterSpec extends MorphirBaseSpec {

  val sampleIR = IR(
    valueSpecifications = Map.empty,
    valueDefinitions = Map.empty,
    typeSpecifications = Map(recordTypeName -> recordTypeAliasSpecification),
    typeConstructors = Map(
      savingsAccountTypeName  -> savingsAccountTypeConstructor,
      checkingAccountTypeName -> checkingAccountTypeConstructor
    )
  )
  def evaluate(value: RawValue): Any = Interpreter.evaluate(value, sampleIR, Map.empty)

  def spec = suite("Interpreter")(
    suite("native functions")(
      suite("addition")(
        test("Should evaluate correctly") {
          assertTrue(
            evaluate(additionExample) == Right(new BigInteger("3"))
          )
        }
      ),
      suite("subtraction")(
        test("Should evaluate correctly") {
          assertTrue(evaluate(subtractionExample) == Right(new BigInteger("-1")))
        }
      )
    ),
    suite("tuple case")(
      test("Should evaluate correctly") {
        assertTrue(evaluate(tupleCaseExample) == Right((new BigInteger("1"), new BigInteger("2"))))
      }
    ),
    suite("list case")(
      test("Should evaluate correctly") {
        assertTrue(evaluate(listCaseExample) == Right(List("hello", "world")))
      }
    ),
    suite("if then else case")(
      test("Should evaluate correctly") {
        assertTrue(evaluate(ifThenElseCaseExample) == Right("no"))
      }
    ),
    suite("record case")(
      test("Should evaluate correctly") {
        assertTrue(
          evaluate(recordCaseExample) == Right(
            Map(
              Name.unsafeMake(List("field", "a")) -> "hello",
              Name.unsafeMake(List("field", "b")) -> new BigInteger("2")
            )
          )
        )
      },
      test("Should update correctly") {
        assertTrue(
          evaluate(recordCaseUpdateExample) == Right(
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
        assertTrue(evaluate(letIntroduceMultipleExample) == Right(new BigInteger("42")))
      },
      test("Multiple bindings where earlier binding refers to later definition") {
        assertTrue(evaluate(letIntroduceOutOfOrderExample) == Right(new BigInteger("44")))
      },
      test("recursive let definition example") {
        assertTrue(evaluate(letRecExample) == Right(new BigInteger("6")))
      },
      test("Static scoping example") {
        assertTrue(evaluate(staticScopingExample) == Right("static"))
      }
    ),
    suite("let non recursion case")(
      test("Let destructor case") {
        assertTrue(evaluate(letDestructExample) == Right("red"))
      }
    ),
    suite("apply case")(
      test("Apply field function") {
        assertTrue(evaluate(applyFieldFunction) == Right("hello"))
      },
      test("Apply lambda with wildcard") {
        assertTrue(evaluate(applyWithWildCard) == Right(new BigInteger("42")))
      },
      test("Lambda defined in let") {
        assertTrue(evaluate(lambdaExample) == Right(new BigInteger("66")))
      }
    ),
    suite("constructor case")(
      test("Should evaluate correctly XYZ") {
        assertTrue(
          evaluate(constructorExample) == Right(
            GenericCaseClass.fromFields(recordTypeName, Name("name") -> "Adam", Name("age") -> new BigInteger("42"))
          )
        )
      },
      test("Custom type should evaluate correctly") {
        assertTrue(
          evaluate(savingsAccountConstructorExample) == Right(
            GenericCaseClass.fromFields(
              savingsAccountTypeName,
              Name("arg1") -> "Adam"
            )
          ),
          evaluate(checkingAccountConstructorExample) == Right(
            GenericCaseClass.fromFields(
              checkingAccountTypeName,
              Name("arg1") -> "Brad",
              Name("arg2") -> new BigInteger("10000")
            )
          )
        )
      }
    ),
    suite("pattern matching")(
      suite("literal")(),
      suite("wildcard")(
        test("Should evaluate correctly") {
          assertTrue(evaluate(patternMatchWildcardCaseExample) == Right(new BigInteger("100")))
        }
      ),
      suite("as")(
        test("Should evaluate correctly") {
          assertTrue(evaluate(patternMatchAsCaseExample) == Right(new BigInteger("42")))
        }
      ),
      suite("as with literal")(
        test("Should evaluate correctly") {
          assertTrue(evaluate(patternMatchAsCaseComplexExample) == Right(new BigInteger("14")))
        }
      ),
      suite("tuple")(
        test("Should evaluate correctly") {
          assertTrue(evaluate(patternTupleCaseExample) == Right(new BigInteger("107")))
        }
      ),
      suite("singleton tuple")(
        test("Should evaluate correctly") {
          assertTrue(evaluate(patternTupleOneCaseExample) == Right("singleton tuple"))
        }
      ),
      suite("singleton non match tuple")(
        test("Should evaluate correctly") {
          assertTrue(evaluate(patternTupleOneCaseCounterExample) == Right("right"))
        }
      ),
      suite("constructor")(
        test("Should evaluate correctly") {
          assertTrue(evaluate(patternConstructorCaseExample) == Right(new BigInteger("10000")))
        }
      ),
      suite("head tail list")(
        test("Should evaluate correctly") {
          assertTrue(evaluate(patternHeadTailCaseExample) == Right(List("world")))
        }
      ),
      suite("empty list")(
        test("Should evaluate correctly") {
          assertTrue(evaluate(patternMatchEmptyListCaseExample) == Right("empty list"))
        }
      ),
      suite("unit")(
        test("Should evaluate correctly") {
          assertTrue(evaluate(patternUnitCaseExample) == Right("right"))
        }
      )
      // a @ b @ 1
      // a @List( b, 2)
      //
    )
  )

}
