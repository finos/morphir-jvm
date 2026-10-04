package morphir.codegen.tasty

import zio.test.*

/** Verifies supported Scala-to-Morphir mappings.
  *
  * Most supported mappings are tested via full JSON equivalence between a one-to-one Elm fixture project and the
  * equivalent Scala .tasty file. BigDecimal division is the one exception: Scala maps `/` to
  * `morphir.SDK.decimal.div.unsafe`, while the Morphir Elm SDK exposes `Decimal.div : Decimal -> Decimal -> Maybe
  * Decimal`, so that case is covered with a targeted Scala-side mapping assertion.
  */
object SupportedFunctionEquivalenceSpec extends TastyEquivalenceSuite:

  private val arithmeticCases = List(
    arithmeticCase("add", "Add", "add"),
    arithmeticCase("subtract", "Subtract", "subtract"),
    arithmeticCase("multiply", "Multiply", "multiply"),
    arithmeticCase("isPositive", "IsPositive", "is-positive"),
    arithmeticCase("clamp", "Clamp", "clamp"),
    arithmeticCase("integerDivide", "IntegerDivide", "integer-divide")
  )

  private val literalCases = List(
    arithmeticCase("booleanLiteral", "BooleanLiteral", "boolean-literal"),
    arithmeticCase("booleanAnd", "BooleanAnd", "boolean-and"),
    arithmeticCase("booleanOr", "BooleanOr", "boolean-or"),
    arithmeticCase("doubleLiteral", "DoubleLiteral", "double-literal"),
    arithmeticCase("charLiteral", "CharLiteral", "char-literal"),
    arithmeticCase("stringLiteral", "StringLiteral", "string-literal"),
    arithmeticCase("doubleAdd", "DoubleAdd", "double-add"),
    arithmeticCase("longLiteral", "LongLiteral", "long-literal"),
    arithmeticCase("longAdd", "LongAdd", "long-add")
  )

  private val helperCases = List(
    arithmeticCase("helperCall", "HelperCall", "helper-call"),
    arithmeticCase("curriedHelperCall", "CurriedHelperCall", "curried-helper-call")
  )

  private val decimalCases = List(
    arithmeticCase("decimalAdd", "DecimalAdd", "decimal-add"),
    arithmeticCase("decimalSubtract", "DecimalSubtract", "decimal-subtract"),
    arithmeticCase("decimalMultiply", "DecimalMultiply", "decimal-multiply"),
    arithmeticCase("decimalLt", "DecimalLt", "decimal-lt"),
    arithmeticCase("decimalLte", "DecimalLte", "decimal-lte"),
    arithmeticCase("decimalGt", "DecimalGt", "decimal-gt"),
    arithmeticCase("decimalGte", "DecimalGte", "decimal-gte")
  )

  private val scalarCases =
    arithmeticCases ++ literalCases ++ helperCases ++ decimalCases

  private val foundationalTypeGapCases = List(
    arithmeticCase("unitBasics", "UnitBasics", "unit-basics"),
    arithmeticCase("smallIntBasics", "SmallIntBasics", "small-int-basics")
  )

  private val operatorCases = List(
    arithmeticCase("booleanEqual", "BooleanEqual", "boolean-equal"),
    arithmeticCase("intEqual", "IntEqual", "int-equal"),
    arithmeticCase("stringNotEqual", "StringNotEqual", "string-not-equal"),
    arithmeticCase("booleanNot", "BooleanNot", "boolean-not"),
    arithmeticCase("negateInt", "NegateInt", "negate-int"),
    arithmeticCase("negateFloat", "NegateFloat", "negate-float")
  )

  private val numericHelperCases = List(
    arithmeticCase("absInt", "AbsInt", "abs-int"),
    arithmeticCase("absFloat", "AbsFloat", "abs-float"),
    arithmeticCase("minInt", "MinInt", "min-int"),
    arithmeticCase("maxInt", "MaxInt", "max-int"),
    arithmeticCase("toFloatInt", "ToFloatInt", "to-float-int"),
    arithmeticCase("truncateFloat", "TruncateFloat", "truncate-float")
  )

  private val collectionCases = List(
    arithmeticCase("listEmpty", "ListEmpty", "list-empty"),
    arithmeticCase("listLiteral", "ListLiteral", "list-literal"),
    arithmeticCase("nestedListLiteral", "NestedListLiteral", "nested-list-literal"),
    arithmeticCase("listLength", "ListLength", "list-length"),
    arithmeticCase("listMapIncrement", "ListMapIncrement", "list-map-increment"),
    arithmeticCase("listFilterPositive", "ListFilterPositive", "list-filter-positive"),
    arithmeticCase("listFlatMapExpand", "ListFlatMapExpand", "list-flatmap-expand"),
    arithmeticCase("listFoldSum", "ListFoldSum", "list-fold-sum"),
    arithmeticCase("listMapTupleMatch", "ListMapTupleMatch", "list-map-tuple-match"),
    arithmeticCase("listCollectSomeIncrement", "ListCollectSomeIncrement", "list-collect-some-increment"),
    arithmeticCase("seqLiteral", "SeqLiteral", "seq-literal"),
    arithmeticCase("seqMapIncrement", "SeqMapIncrement", "seq-map-increment"),
    arithmeticCase("seqFlatMapExpand", "SeqFlatMapExpand", "seq-flatmap-expand"),
    arithmeticCase("seqFoldSum", "SeqFoldSum", "seq-fold-sum"),
    arithmeticCase("mapLiteral", "MapLiteral", "map-literal"),
    arithmeticCase("mapPassThrough", "MapPassThrough", "map-pass-through"),
    arithmeticCase("mapGet", "MapGet", "map-get"),
    arithmeticCase("forYieldIncrement", "ForYieldIncrement", "for-yield-increment"),
    arithmeticCase("forYieldExpand", "ForYieldExpand", "for-yield-expand")
  )

  private val tupleCases = List(
    arithmeticCase("tupleLiteral", "TupleLiteral", "tuple-literal"),
    arithmeticCase("tuplePassThrough", "TuplePassThrough", "tuple-pass-through"),
    arithmeticCase("extractFirst", "ExtractFirst", "extract-first"),
    arithmeticCase("extractFirstOf3", "ExtractFirstOf3", "extract-first-of-3"),
    arithmeticCase("extractFirstOf4", "ExtractFirstOf4", "extract-first-of-4"),
    arithmeticCase("extractSecond", "ExtractSecond", "extract-second"),
    arithmeticCase("tupleDestructureAdd", "TupleDestructureAdd", "tuple-destructure-add"),
    arithmeticCase("tupleDestructureAdd3", "TupleDestructureAdd3", "tuple-destructure-add-3"),
    arithmeticCase("tupleDestructureAdd4", "TupleDestructureAdd4", "tuple-destructure-add-4")
  )

  private val bindingCases = List(
    arithmeticCase("localVal", "LocalVal", "local-val"),
    arithmeticCase("localValChain", "LocalValChain", "local-val-chain"),
    arithmeticCase("localValHelperCall", "LocalValHelperCall", "local-val-helper-call")
  )

  private val maybeCases = List(
    arithmeticCase("maybeJust", "MaybeJust", "maybe-just"),
    arithmeticCase("maybeNothing", "MaybeNothing", "maybe-nothing"),
    arithmeticCase("maybePositive", "MaybePositive", "maybe-positive")
  )

  private val matchCases = List(
    arithmeticCase("booleanLiteralMatch", "BooleanLiteralMatch", "boolean-literal-match"),
    arithmeticCase("intLiteralMatch", "IntLiteralMatch", "int-literal-match"),
    arithmeticCase("floatLiteralMatch", "FloatLiteralMatch", "float-literal-match"),
    arithmeticCase("stringLiteralMatch", "StringLiteralMatch", "string-literal-match"),
    arithmeticCase("maybeMatchDefault", "MaybeMatchDefault", "maybe-match-default"),
    arithmeticCase("maybeMatchIncrement", "MaybeMatchIncrement", "maybe-match-increment"),
    arithmeticCase("maybeMatchMap", "MaybeMatchMap", "maybe-match-map")
  )

  private val adtCases = List(
    arithmeticMultiFileCase("simpleColorAdt", List("Color", "MatchRed", "WrapRed"), "simple-color-adt"),
    arithmeticMultiFileCase(
      "colorWithValueAdt",
      List("ColorWithValue", "GetIntensity", "RedFortyTwo"),
      "color-with-value-adt"
    ),
    arithmeticMultiFileCase(
      "colorWithTwoValuesAdt",
      List("ColorWithTwoValues", "GetNamedIntensity", "NamedRed"),
      "color-with-two-values-adt"
    ),
    arithmeticMultiFileCase(
      "colorWithThreeValuesAdt",
      List("ColorWithThreeValues", "GetPrimaryValue", "NamedRedPriority"),
      "color-with-three-values-adt"
    )
  )

  private val equivalenceCases =
    scalarCases ++
      foundationalTypeGapCases ++
      operatorCases ++
      numericHelperCases ++
      collectionCases ++
      tupleCases ++
      bindingCases ++
      maybeCases ++
      matchCases ++
      adtCases

  def spec = suite("SupportedFunctionEquivalenceSpec")(
    equivalenceSpecs(equivalenceCases)(
      testName = equivalenceCase => s"${equivalenceCase.caseName}: Scala and Elm distributions are identical",
      descriptionPrefix = "fixture case"
    ),
    test("decimalDivide: Scala IR uses morphir.SDK.decimal.div.unsafe") {
      val valueDefinition = firstValueDefinition(
        scalaIR(List("arithmetic/DecimalDivide.tasty"), "Scala IR for arithmetic/DecimalDivide.tasty")
      )
      val outputType =
        valueDefinition.hcursor.downField("outputType").focus.getOrElse(throw new AssertionError("Missing outputType"))
      val body = valueDefinition.hcursor.downField("body").focus.getOrElse(throw new AssertionError("Missing body"))

      assertTrue(
        !outputType.noSpaces.contains("\"maybe\""),
        body.noSpaces.contains("[\"div\",\"unsafe\"]")
      )
    }
  ) @@ TestAspect.sequential
