package zio.morphir.ir.testing

import zio.morphir.ir.Name
import zio.{Chunk, ZEnvironment}
import zio.morphir.ir.TypeModule.Type
import zio.morphir.ir.ValueModule.{Value, ValueDefinition}
import zio.morphir.ir.NativeFunction
import zio.morphir.ir.{FQName, Path, TypeModule}
import zio.morphir.IR.TypeConstructorInfo
import zio.morphir.Dsl
import zio.morphir.syntax.ValueSyntax
import zio.morphir.syntax.TypeSyntax

object CaseExample extends ValueSyntax with TypeSyntax {

  // /x = if (foo) y else 0
  // y = if (!foo) x else 0
  val letIntroduceMultipleExample: Value[Any] = letRecursion(
    Map(
      Name.fromString("x") -> ValueDefinition.fromLiteral(int(20)), // lit(20)
      Name.fromString("y") -> ValueDefinition.fromLiteral(int(22))
    ),
    nativeApply(
      NativeFunction.Addition,
      Chunk(variable("x"), variable("y"))
    )
  )

  val letIntroduceOutOfOrderExample: Value[Any] = letRecursion(
    Map(
      Name.fromString("x") ->
        nativeApply(
          NativeFunction.Addition,
          Chunk(
            variable("y"),
            int(22)
          )
        ).toDefinition,
      Name.fromString("y") -> ValueDefinition.fromLiteral(int(22))
    ),
    variable("x")
  )

  val applyFieldFunction: Value[Any] =
    Dsl.apply(fieldFunction(Name.fromString("fieldA")), recordCaseExample)

  val additionExample: Value[Any] =
    letDefinition(
      Name("x"),
      ValueDefinition.fromLiteral(int(1)),
      letDefinition(
        Name("y"),
        ValueDefinition.fromLiteral(int(2)),
        nativeApply(
          NativeFunction.Addition,
          Chunk(variable("x"), variable("y"))
        )
      )
    )

  val subtractionExample: Value[Any] =
    letDefinition(
      Name("x"),
      ValueDefinition.fromLiteral(int(1)),
      letDefinition(
        Name("y"),
        ValueDefinition.fromLiteral(int(2)),
        nativeApply(
          NativeFunction.Subtraction,
          Chunk(variable(Name("x")), variable(Name("y")))
        )
      )
    )

  val tupleCaseExample: Value[Any] =
    tuple(
      literal(1),
      literal(2)
    )

  val listCaseExample: Value[Any] =
    list(
      Chunk(
        literal("hello"),
        literal("world")
      )
    )
  val ifThenElseCaseExample: Value[Any] =
    ifThenElse(
      condition = literal(false),
      thenBranch = literal("yes"),
      elseBranch = literal("no")
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

  val recordCaseUpdateExample = {
    updateRecord(
      recordCaseExample,
      Chunk(
        Name("fieldB") -> Dsl.wholeNumber(new java.math.BigInteger("3"))
      )
    )
  }

  val patternMatchWildcardCaseExample =
    Dsl.patternMatch(
      Dsl.wholeNumber(new java.math.BigInteger("42")),
      wildcardPattern -> Dsl.wholeNumber(
        new java.math.BigInteger("100")
      )
    )

  val patternMatchAsCaseExample =
    Dsl.patternMatch(
      Dsl.wholeNumber(new java.math.BigInteger("42")),
      asPattern(wildcardPattern, Name.fromString("x")) -> Dsl.variable(Name.fromString("x"))
    )

  val patternMatchEmptyListCaseExample =
    Dsl.patternMatch(
      list(Chunk()),
      emptyListPattern -> literal("empty list")
    )

  val patternHeadTailCaseExample =
    Dsl.patternMatch(
      listCaseExample,
      headTailPattern(
        literalPattern("hello"),
        asPattern(wildcardPattern, Name("tail"))
      ) -> variable("tail")
    )

  val patternTupleOneCaseExample =
    Dsl.patternMatch(
      tuple(literal("singleton tuple")),
      tuplePattern(asPattern(wildcardPattern, Name("x"))) -> variable(Name("x"))
    )

  val patternTupleOneCaseCounterExample =
    Dsl.patternMatch(
      literal("singleton tuple"),
      tuplePattern(wildcardPattern) -> literal("wrong"),
      wildcardPattern               -> literal("right")
    )

  val patternTupleCaseExample =
    Dsl.patternMatch(
      tupleCaseExample,
      tuplePattern(
        wildcardPattern,
        wildcardPattern
      ) -> Dsl.wholeNumber(new java.math.BigInteger("107"))
    )

  lazy val patternConstructorCaseExample =
    Dsl.patternMatch(
      checkingAccountConstructorExample,
      constructorPattern(
        checkingAccountTypeName,
        Chunk(wildcardPattern, asPattern(wildcardPattern, Name("x")))
      ) -> variable(Name("x"))
    )

  val patternUnitCaseExample =
    Dsl.patternMatch(
      unit,
      emptyListPattern -> literal("wrong"),
      unitPattern      -> literal("right")
    )

  val letDestructExample =
    destructure(
      tuplePattern(asPattern(wildcardPattern, Name("x")), asPattern(wildcardPattern, Name("y"))),
      tuple(string("red"), string("blue")),
      variable("x")
    )
  val staticScopingExample =
    letDefinition(
      Name("x"),
      ValueDefinition.fromLiteral(string("static")),
      letRecursion(
        Map(Name("y") -> variable(Name("x")).toDefinition),
        letDefinition(Name("x"), ValueDefinition.fromLiteral(string(("dynamic"))), variable(Name("y")))
      )
    )
  val letRecExample =
    letRecursion(
      Map(
        Name.fromString("x") -> ifThenElse(
          condition = literal(false),
          thenBranch = variable("y"),
          elseBranch = literal(3)
        ).toDefinition,
        Name.fromString("y") ->
          ifThenElse(
            condition = literal(false),
            thenBranch = literal(2),
            elseBranch = variable("x")
          ).toDefinition
      ),
      nativeApply(
        NativeFunction.Addition,
        Chunk(
          variable("x"),
          variable("y")
        )
      )
    )

  // (valueDefinitions: Map[Name, Self], inValue: Self)

  // example : letrec (x, y) = if (cond) then (0, x) else (y, 0)

  /**
   * letRec x -> 3 y -> 4 x + y
   */

  val patternMatchAsCaseComplexExample =
    Dsl.patternMatch(
      Dsl.wholeNumber(new java.math.BigInteger("7")),
      asPattern(
        literalPattern(8),
        Name.fromString("x")
      ) ->
        nativeApply(
          NativeFunction.Subtraction,
          Chunk(
            variable(Name.fromString("x")),
            variable(Name.fromString("x"))
          )
        ),
      asPattern(
        literalPattern(7),
        Name.fromString("x")
      ) ->
        nativeApply(
          NativeFunction.Addition,
          Chunk(
            variable(Name.fromString("x")),
            variable(Name.fromString("x"))
          )
        )
    )

  // { case _ => 42}()

  val applyWithWildCard =
    Dsl.apply(lambda(wildcardPattern, wholeNumber(new java.math.BigInteger("42"))), Dsl.unit)

  val lambdaExample = letDefinition(
    Name("foo"),
    lambda(
      asPattern(wildcardPattern, Name("x")),
      nativeApply(
        NativeFunction.Addition,
        Chunk(
          variable("x"),
          variable("x")
        )
      )
    ).toDefinition,
    Dsl.apply(variable("foo"), literal(33))
  )

  val personName = zio.morphir.ir.FQName(zio.morphir.ir.Path(Name("")), zio.morphir.ir.Path(Name("")), Name("Person"))
  lazy val recordTypeName =
    zio.morphir.ir.FQName(
      zio.morphir.ir.Path(Name("Morphir.SDK")),
      zio.morphir.ir.Path(Name("Morphir.SDK")),
      Name("RecordType")
    )

  lazy val recordType = defineRecord(
    defineField(Name("name"), Type.unit),
    defineField(Name("age"), Type.unit)
  )

  lazy val recordTypeAliasSpecification = TypeModule.Specification.TypeAliasSpecification[Any](
    typeParams = Chunk.empty,
    expr = recordType,
    annotations = ZEnvironment.empty
  )

  lazy val accountTypeName = FQName(
    Path(Name("Morphir.SDK")),
    Path(Name("Morphir.SDK.Account")),
    Name("Account")
  )

  lazy val savingsAccountTypeName = FQName(
    Path(Name("Morphir.SDK")),
    Path(Name("Morphir.SDK.Account")),
    Name("SavingsAccount")
  )
  lazy val checkingAccountTypeName = FQName(
    Path(Name("Morphir.SDK")),
    Path(Name("Morphir.SDK.Account")),
    Name("CheckingAccount")
  )

  lazy val savingsAccountTypeConstructor = TypeConstructorInfo(
    containingType = accountTypeName,
    typeParams = Chunk.empty,
    typeArgs = Chunk(Name.fromString("arg1") -> defineReference(FQName.fromString("Morphir.SDK.String"), Chunk.empty))
  )

  lazy val checkingAccountTypeConstructor = TypeConstructorInfo(
    containingType = accountTypeName,
    typeParams = Chunk.empty,
    typeArgs = Chunk(
      Name.fromString("arg1") -> defineReference(FQName.fromString(":Morphir.SDK:String"), Chunk.empty),
      Name.fromString("arg2") -> defineReference(FQName.fromString(":Morphir.SDK:Int"), Chunk.empty)
    )
  )

  val constructorExample =
    apply(
      constructor(recordTypeName),
      Chunk(literal("Adam"), literal(42))
    )

  val savingsAccountConstructorExample =
    apply(
      constructor(savingsAccountTypeName),
      Chunk(literal("Adam"))
    )

  val checkingAccountConstructorExample =
    apply(
      constructor(checkingAccountTypeName),
      Chunk(literal("Brad"), literal(10000))
    )

  // tuple ("Adam", 42)
  // record (name: "Adam", age: 42)
  // extensiblerecord (Person((name: "Adam", age: 42)

}
