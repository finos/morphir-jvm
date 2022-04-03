package zio.morphir.testing

import zio.morphir.ir.Name
import zio.Chunk
import zio.morphir.ir.Type.Type
import zio.morphir.ir.Value.{Definition => ValueDefinition, RawValue}
import zio.morphir.ir.NativeFunction
import zio.morphir.ir.{FQName, Path}
import zio.morphir.IR.TypeConstructorInfo
import zio.morphir.Dsl
import zio.morphir.syntax.AllSyntax
import zio.morphir.ir.sdk.{Basics, String => StringModule}

object CaseExample extends AllSyntax {

  // /x = if (foo) y else 0
  // y = if (!foo) x else 0
  val letIntroduceMultipleExample: RawValue = letRecursion(
    Map(
      Name.fromString("x") -> ValueDefinition.fromRawValue(int(20) -> Basics.intType), // lit(20)
      Name.fromString("y") -> ValueDefinition.fromRawValue(int(22) -> Basics.intType)  // lit(22)
    ),
    nativeApply(
      NativeFunction.Addition,
      Chunk(variable("x"), variable("y"))
    )
  )

  val letIntroduceOutOfOrderExample: RawValue = letRecursion(
    Map(
      Name.fromString("x") ->
        nativeApply(
          NativeFunction.Addition,
          Chunk(
            variable("y"),
            int(22)
          )
        ).toDefinition(Basics.intType),
      Name.fromString("y") -> int(22).toDefinition(Basics.intType)
    ),
    variable("x")
  )

  val applyFieldFunction: RawValue =
    Dsl.apply(fieldFunction(Name.fromString("fieldA")), recordCaseExample)

  val additionExample: RawValue =
    letDefinition(
      Name("x"),
      int(1).toDefinition(Basics.intType),
      letDefinition(
        Name("y"),
        int(2).toDefinition(Basics.intType),
        nativeApply(
          NativeFunction.Addition,
          Chunk(variable("x"), variable("y"))
        )
      )
    )

  val subtractionExample: RawValue =
    letDefinition(
      Name("x"),
      int(1).toDefinition(Basics.intType),
      letDefinition(
        Name("y"),
        int(2).toDefinition(Basics.intType),
        nativeApply(
          NativeFunction.Subtraction,
          Chunk(variable(Name("x")), variable(Name("y")))
        )
      )
    )

  val tupleCaseExample =
    tuple(
      literal(1),
      literal(2)
    )

  val listCaseExample =
    list(
      string("hello"),
      string("world")
    )
  val ifThenElseCaseExample: RawValue =
    ifThenElse(
      condition = literal(false),
      thenBranch = string("yes"),
      elseBranch = string("no")
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
      list(),
      emptyListPattern -> string("empty list")
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
      tuple(string("singleton tuple")),
      tuplePattern(asPattern(wildcardPattern, Name("x"))) -> variable(Name("x"))
    )

  val patternTupleOneCaseCounterExample =
    Dsl.patternMatch(
      string("singleton tuple"),
      tuplePattern(wildcardPattern) -> string("wrong"),
      wildcardPattern               -> string("right")
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
      emptyListPattern -> string("wrong"),
      unitPattern      -> string("right")
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
      string("static").toDefinition(StringModule.stringType),
      letRecursion(
        Map(Name("y") -> variable(Name("x")).toDefinition(StringModule.stringType)),
        letDefinition(Name("x"), string(("dynamic")).toDefinition(StringModule.stringType), variable(Name("y")))
      )
    )
  val letRecExample =
    letRecursion(
      Map(
        Name.fromString("x") -> ifThenElse(
          condition = literal(false),
          thenBranch = variable("y"),
          elseBranch = literal(3)
        ).toDefinition(Basics.intType),
        Name.fromString("y") ->
          ifThenElse(
            condition = literal(false),
            thenBranch = literal(2),
            elseBranch = variable("x")
          ).toDefinition(Basics.intType)
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
    ).toDefinition(Basics.intType),
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

  lazy val recordTypeAliasSpecification = zio.morphir.ir.Type.Specification.TypeAliasSpecification[Any](
    typeParams = Chunk.empty,
    expr = recordType
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
      literal("Adam").toRawValue,
      literal(42)
    )

  val savingsAccountConstructorExample =
    apply(
      constructor(savingsAccountTypeName),
      literal("Adam").toRawValue
    )

  val checkingAccountConstructorExample =
    apply(
      constructor(checkingAccountTypeName),
      literal("Brad").toRawValue,
      literal(10000)
    )

  // tuple ("Adam", 42)
  // record (name: "Adam", age: 42)
  // extensiblerecord (Person((name: "Adam", age: 42)

}
