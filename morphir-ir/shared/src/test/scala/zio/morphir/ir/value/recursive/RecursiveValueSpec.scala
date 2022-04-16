package zio.morphir.ir.value.recursive

import zio.Chunk
import zio.morphir.ir.sdk.Basics.{boolType, floatType, intType}
import zio.morphir.ir.sdk.String.stringType
import zio.morphir.ir.{FQName, Gens, Literal => Lit, Name, Type}
import zio.morphir.testing.MorphirBaseSpec
import zio.test._

object RecursiveValueSpec extends MorphirBaseSpec {
  import Value._
  import ValueCase._
  def spec: ZSpec[Environment, Any] = suite("Value Spec")(
    suite("Apply")(
      suite("Attributed")(
        test("It should be possible to create a single argument function application") {
          val attribute   = "int -> string"
          val intToString = reference(stringType, "Morphir.SDK", "Int", "intToString")
          val actual      = apply(attribute, intToString, int(42))

          assertTrue(
            actual == Apply(attribute, intToString, int(42)),
            actual == Value(ApplyCase(attribute, intToString, int(42))),
            actual.attributes == attribute,
            actual.toString == "Morphir.SDK.Int.intToString 42",
            actual.isData == false
          )
        },
        test("It should be possible to create a multi argument function application") {
          val attribute = "int -> int"
          val max       = reference(intType, "Morphir.SDK", "Basics", "max")
          val actual    = apply(attribute, apply("int -> int -> int", max, int(1)), int(2))

          assertTrue(
            actual == Apply(attribute, Apply("int -> int -> int", max, int(1)), int(2)),
            actual.attributes == attribute,
            actual.toString == "Morphir.SDK.Basics.max 1 2",
            actual.isData == false
          )
        }
      ),
      suite("Unattributed")(
        test("It should be possible to create a single argument function application") {
          val intToString = reference("Morphir.SDK", "Int", "intToString")
          val actual      = apply(intToString, int(100))

          assertTrue(
            actual == Apply.Raw(intToString, int(100)),
            actual == Value(ApplyCase((), intToString, int(100))),
            actual.toString == "Morphir.SDK.Int.intToString 100",
            actual.isData == false
          )
        },
        test("It should be possible to create a multi argument function application") {
          val max    = reference("Morphir.SDK", "Basics", "max")
          val actual = apply(apply(max, int(1)), int(2))

          assertTrue(
            actual == Apply.Raw(Apply.Raw(max, int(1)), int(2)),
            actual.toString == "Morphir.SDK.Basics.max 1 2",
            actual.isData == false
          )
        }
      ),
      suite("Currying/Uncurrying")(
        test("It should properly uncurry an application when using uncurryApply") {
          val f                                 = variable("f")
          val a                                 = variable("a")
          val b                                 = variable("b")
          val sut                               = apply(f, a)
          val actual                            = sut.uncurryApply(b)
          val (actualFunction, actualArguments) = actual

          assertTrue(
            actual == (f, a :: b :: Nil),
            actualFunction == f,
            actualArguments == a :: b :: Nil,
            actualArguments.size == 2
          )
        }
      )
    ),
    suite("Constructor")(
      suite("Attributed")(
        test("It should be possible to construct given attributes and a FQ name as a string") {
          val fqName     = "Morphir.SDK:Maybe:Just"
          val attributes = "Maybe"
          val actual     = constructor(attributes, fqName)
          assertTrue(
            actual == Constructor(attributes, fqName),
            actual.attributes == "Maybe",
            actual.toString() == "Morphir.SDK.Maybe.Just",
            actual.isData == true
          )
        },
        test("It should be possible to construct given attributes and a FQName") {
          val fqName     = FQName.fqn("Morphir.SDK", "Maybe", "Just")
          val attributes = "Maybe"
          val actual     = constructor(attributes, fqName)
          assertTrue(
            actual == Constructor(attributes, fqName),
            actual.attributes == "Maybe",
            actual.toString() == "Morphir.SDK.Maybe.Just",
            actual.isData == true
          )
        }
      ),
      suite("Unattributed")(
        test("It should be possible to construct given a FQ name as a string") {
          val fqName = "Morphir:Morphir.SDK.Maybe:Just"
          val actual = constructor(fqName)
          assertTrue(
            actual == Constructor.Raw(fqName),
            actual == Constructor((), fqName),
            actual.attributes == (),
            actual.toString() == "Morphir.Morphir.SDK.Maybe.Just",
            actual.isData == true
          )
        },
        test("It should be possible to construct given attributes and a FQName") {
          val fqName = FQName.fqn("Morphir", "Morphir.SDK.Maybe", "Nothing")
          val actual = constructor(fqName)
          assertTrue(
            actual == Constructor.Raw(fqName),
            actual == Constructor((), fqName),
            actual.attributes == (),
            actual.toString() == "Morphir.Morphir.SDK.Maybe.Nothing",
            actual.isData == true
          )
        }
      )
    ),
    suite("Destructure")(
      suite("Attributed")(
        test("It should be possible to create given attrributes and a simple pattern") {
          val attributes = Type.tuple(intType, stringType)
          val pat        = tuplePattern(attributes, asAlias(intType, "a"), asAlias(stringType, "b"))
          val c          = variable(attributes, "c")
          val in         = variable(intType, "a")
          val actual     = destructure(attributes, pat, c, in)
          assertTrue(
            actual == Destructure(attributes, pat, c, in),
            actual == letDestruct(attributes, pat, c, in),
            actual.attributes == attributes,
            actual.toString == "let (a, b) = c in a",
            actual.isData == false
          )
        }
      ),
      suite("Unattributed")(
        test("It should be possible to create a simple pattern") {
          val pat    = headTailPattern(asAlias("head"), asAlias("tail"))
          val myList = variable("myList")
          val in     = variable("tail")
          val actual = destructure(pat, myList, in)
          assertTrue(
            actual == Destructure.Raw(pat, myList, in),
            actual == letDestruct(pat, myList, in),
            actual.attributes == (),
            actual.toString == "let head :: tail = myList in tail",
            actual.isData == false
          )
        }
      )
    ),
    suite("Field")(
      suite("Attributed")(
        test("It should be possible to construct a field access given attributes a subject/target and a field name") {
          val subject = variable("person")
          val actual  = field(stringType, subject, "firstName")
          assertTrue(
            actual == Field(stringType, subject, "firstName"),
            actual.attributes == stringType,
            actual.toString == "person.firstName",
            actual.isData == false
          )
        },
        test("It should be possible to construct a field access given attributes a subject/target and a field name") {
          val subject   = variable("person")
          val firstName = Name.fromString("firstName")
          val actual    = field(stringType, subject, firstName)
          assertTrue(
            actual == Field(stringType, subject, firstName),
            actual.attributes == stringType,
            actual.toString == "person.firstName",
            actual.isData == false
          )
        }
      ),
      suite("Unattributed")(
        test("It should be possible to construct a field access given a subject/target and a field name") {
          val subject = variable("person")
          val actual  = field(subject, "firstName")
          assertTrue(
            actual == Field.Raw(subject, "firstName"),
            actual.toString == "person.firstName",
            actual.isData == false
          )
        }
      )
    ),
    suite("FieldFunction")(
      suite("Attributed")(
        test("It should be possible to construct given attributes and a field name as a string") {
          val fieldName = "DayOfMonth"
          val actual    = fieldFunction(intType, fieldName)
          assertTrue(
            actual.toString == ".dayOfMonth",
            actual == FieldFunction(intType, fieldName),
            actual.isData == false
          )
        },
        test("It should be possible to construct given attributes and a field name") {
          val fieldName = Name.fromString("DayOfMonth")
          val actual    = fieldFunction(intType, fieldName)
          assertTrue(
            actual.toString == ".dayOfMonth",
            actual == FieldFunction(intType, fieldName),
            actual.isData == false
          )
        }
      ),
      suite("Unattributed")(
        test("It should be possible to construct given a field name as a string") {
          val fieldName = "DayOfMonth"
          val actual    = fieldFunction(fieldName)
          assertTrue(
            actual.toString == ".dayOfMonth",
            actual == FieldFunction.Raw(fieldName),
            actual == FieldFunction((), fieldName),
            actual.isData == false
          )
        },
        test("It should be possible to construct given a field name") {
          val fieldName = Name.fromString("DayOfMonth")
          val actual    = fieldFunction(fieldName)
          assertTrue(
            actual.toString == ".dayOfMonth",
            actual == FieldFunction.Raw(fieldName),
            actual == FieldFunction((), fieldName),
            actual.isData == false
          )
        }
      )
    ),
    suite("IfThenElse")(
      suite("Attributed")(
        test("It should be possible to create with attributes") {
          val condition = variable(boolType, "condition")
          val actual    = ifThenElse(intType, condition, variable(intType, "a"), variable(intType, "b"))
          assertTrue(
            actual == IfThenElse(intType, condition, variable(intType, "a"), variable(intType, "b")),
            actual.attributes == intType,
            actual.toString == "if condition then a else b",
            actual.isData == false
          )
        }
      ),
      suite("Unattributed")(
        test("It should be possible to create") {
          val condition = variable("condition")
          val actual    = ifThenElse(condition, variable("a"), variable("b"))
          assertTrue(
            actual == IfThenElse.Raw(condition, variable("a"), variable("b")),
            actual.toString == "if condition then a else b",
            actual.isData == false
          )
        }
      )
    ),
    suite("Lambda")(
      suite("Attributed")(
        test("It should be possible to construct given attributes, an argument pattern, and a body") {
          val attributes = Type.tuple(intType, intType)
          val pat        = tuplePattern(attributes, asAlias(intType, "l"), asAlias(stringType, "r"))
          val body = apply(
            intType,
            apply(intType, reference(intType, "Morphir.SDK:Basics:add"), variable(intType, "l")),
            variable(intType, "r")
          )
          val actual = lambda(attributes, pat, body)
          assertTrue(
            actual == Lambda(attributes, pat, body),
            actual.attributes == attributes,
            actual.toString == "(\\(l, r) -> Morphir.SDK.Basics.add l r)",
            actual.isData == false
          )
        }
      ),
      suite("Unattributed")(
        test("It should be possible to construct given an argument pattern and a body") {
          val pat    = tuplePattern(asAlias("l"), asAlias("r"))
          val body   = apply(apply(reference("Morphir.SDK:Basics:multiply"), variable("l")), variable("r"))
          val actual = lambda(pat, body)
          assertTrue(
            actual == Lambda.Raw(pat, body),
            actual == Lambda((), pat, body),
            actual.toString == "(\\(l, r) -> Morphir.SDK.Basics.multiply l r)",
            actual.isData == false
          )
        }
      )
    ),
    suite("LetDefinition")(
      suite("Attributed")(
        test("It should be possible to construct a let definition that is attributed") {
          val a      = variable(boolType, "a")
          val b      = variable(boolType, "b")
          val actual = letDef(boolType, "a", Definition(boolType, b), a)
          assertTrue(
            actual == LetDefinition(boolType, "a", Definition(boolType, b), a),
            actual == let(boolType, "a", Definition()(boolType)(b), a),
            actual.attributes == boolType,
            actual.toString == "let a = b in a",
            actual.isData == false
          )
        }
      ),
      suite("Unattributed")(
        test("It should be possible to construct a let definition that is not attributed") {
          val a          = variable("a")
          val b          = variable("b")
          val definition = Definition.Raw()(boolType)(b)
          val actual     = letDef("a", definition, a)
          assertTrue(
            actual == LetDefinition.Raw("a", definition, a),
            actual == let("a", Definition.Raw()(boolType)(b), a),
            actual.toString == "let a = b in a",
            actual.isData == false
          )
        }
      ),
      suite("Typed")(
        test("It should be possible to use a convenient let constructor for int values") {
          val actual = let("n", 42, variable(intType, "n"))
          assertTrue(
            actual == LetDefinition(intType, "n", Definition.fromLiteral(Lit.int(42)), variable(intType, "n")),
            actual.attributes == intType,
            actual.toString == "let n = 42 in n",
            actual.isData == false
          )
        }
      ),
      suite("Using StringExtensions")(
        test("It should be possible to define a let using := on a int") {
          val varName    = "n"
          val unboundLet = varName := 42
          val actual     = unboundLet.bind(variable(varName, unboundLet.valueDefinition.body.attributes))
          assertTrue(
            unboundLet.toString() == "let n = 42",
            actual.toString() == "let n = 42 in n"
          )
        },
        test("It should be possible to define a let using := on a string") {
          val varName    = "name"
          val unboundLet = varName := "Frank"
          val actual     = unboundLet.bind(variable(varName, unboundLet.valueDefinition.body.attributes))
          assertTrue(
            unboundLet.toString() == "let name = \"Frank\"",
            actual.toString() == "let name = \"Frank\" in name",
            actual == (unboundLet in variable(varName, unboundLet.valueDefinition.body.attributes)),
            actual == ((varName := "Frank") in variable(varName, stringType))
          )
        }
      )
    ),
    suite("LetRecursion")(
      suite("Attributed")(
        test("It should be possible to construct when attributed") {
          val a      = variable(boolType, "a")
          val b      = variable(boolType, "b")
          val aDef   = Definition(boolType, b)
          val bDef   = Definition(boolType, a)
          val actual = letRec(boolType, Map(Name.fromString("a") -> aDef, Name.fromString("b") -> bDef), a)
          assertTrue(
            actual == LetRecursion(boolType, Map(Name.fromString("a") -> aDef, Name.fromString("b") -> bDef), a),
            actual == letRec(boolType, "a" -> aDef, "b" -> bDef)(a),
            actual.attributes == boolType,
            actual.toString == "let a = b; b = a in a",
            actual.isData == false
          )
        }
      ),
      suite("Unattributed")(
        test("It should be possible to construct when not attributed") {
          val a      = variable("a")
          val b      = variable("b")
          val aDef   = Definition.Raw(boolType, b)
          val bDef   = Definition.Raw(boolType, a)
          val actual = letRec(Map(Name.fromString("a") -> aDef, Name.fromString("b") -> bDef), a)
          assertTrue(
            actual == LetRecursion.Raw("a" -> aDef, "b" -> bDef)(a),
            actual == letRec("a" -> aDef, "b" -> bDef)(a),
            actual.toString == "let a = b; b = a in a",
            actual.isData == false
          )
        }
      )
    ),
    suite("List")(
      suite("Attributed")(
        test("It should be possible to create an empty list with only attributes") {
          val actual = list(intType)
          assertTrue(
            actual == List(intType),
            actual.attributes == intType,
            actual.toString == "[]",
            actual.isData == true
          )
        },
        test("It should be possible to create a list with only attributes and a single element") {
          val element = decimal(BigDecimal(3.99))
          val actual  = list(floatType, element)
          assertTrue(
            actual == List(floatType, element),
            actual == List(floatType, Chunk(element)),
            actual.attributes == floatType,
            actual.toString == "[3.99]",
            actual.isData == true
          )
        },
        test("It should be possible to create a list with attributes and multiple elements") {
          val element1 = decimal(BigDecimal(3.99))
          val element2 = decimal(BigDecimal(4.99))
          val element3 = decimal(BigDecimal(5.99))
          val element4 = decimal(BigDecimal(6.99))
          val actual   = list(floatType, element1, element2, element3, element4)
          assertTrue(
            actual == List(floatType, element1, element2, element3, element4),
            actual == List(floatType, Chunk(element1, element2, element3, element4)),
            actual.attributes == floatType,
            actual.toString == "[3.99, 4.99, 5.99, 6.99]",
            actual.isData == true
          )
        }
      ),
      suite("Unattributed")(
        test("It should be possible to create an empty list") {
          val actual = list()
          assertTrue(
            actual == List.Raw(),
            actual == List((), Chunk.empty),
            actual.attributes == (),
            actual.toString == "[]",
            actual.isData == true
          )
        },
        test("It should be possible to create a list with a single element") {
          val element = decimal(BigDecimal(3.99))
          val actual  = list(element)
          assertTrue(
            actual == List.Raw(element),
            actual == List((), Chunk(element)),
            actual.attributes == (),
            actual.toString == "[3.99]",
            actual.isData == true
          )
        },
        test("It should be possible to create a list with multiple elements") {
          val element1 = decimal(BigDecimal(3.99))
          val element2 = decimal(BigDecimal(4.99))
          val element3 = decimal(BigDecimal(5.99))
          val element4 = decimal(BigDecimal(6.99))
          val actual   = list(element1, element2, element3, element4)
          assertTrue(
            actual == List.Raw(element1, element2, element3, element4),
            actual == List((), Chunk(element1, element2, element3, element4)),
            actual.attributes == (),
            actual.toString == "[3.99, 4.99, 5.99, 6.99]",
            actual.isData == true
          )
        }
      )
    ),
    suite("Literal")(
      suite("Attributed")(
        test("It should be possible to construct given attributes and a literal value") {
          check(Gens.literal) { givenLiteral =>
            val inferredType = givenLiteral.inferredType
            val actual       = literal(inferredType, givenLiteral)
            assertTrue(
              actual.toString == givenLiteral.toString(),
              actual.attributes == inferredType,
              actual == Literal(inferredType, givenLiteral),
              actual.isData == true
            )
          }
        }
      ),
      suite("Unattributed")(
        test("It should be possible to construct given a literal value") {
          check(Gens.literal) { givenLiteral =>
            val actual = literal(givenLiteral)
            assertTrue(
              actual.toString == givenLiteral.toString(),
              actual.attributes == (),
              actual == Literal.Raw(givenLiteral),
              actual == Literal((), givenLiteral),
              actual.isData == true
            )
          }
        }
      )
    ),
    suite("PatternMatch")(
      suite("Attributed")(
        test("It should be possible to construct given attributes, a value, and a Chunk of cases") {
          val flag      = variable(boolType, "flag")
          val trueCase  = truePattern(boolType)  -> string("Yes")
          val falseCase = falsePattern(boolType) -> string("No")
          val actual = patternMatch(
            boolType,
            flag,
            Chunk(trueCase, falseCase)
          )
          assertTrue(
            actual == PatternMatch(boolType, flag, Chunk(trueCase, falseCase)),
            actual == PatternMatch(boolType, flag, trueCase, falseCase),
            actual.attributes == boolType,
            actual.toString == "case flag of True -> \"Yes\"; False -> \"No\"",
            actual.isData == false
          )
        }
      ),
      suite("Unattributed")(
        test("It should be possible to construct given a value and a Chunk of cases") {
          val toggle    = variable(boolType, "toggle")
          val trueCase  = truePattern(boolType)  -> string("On")
          val falseCase = falsePattern(boolType) -> string("Off")
          val actual    = patternMatch(toggle, Chunk(trueCase, falseCase))
          assertTrue(
            actual == PatternMatch.Raw(toggle, Chunk(trueCase, falseCase)),
            actual == PatternMatch.Raw(toggle, trueCase, falseCase),
            actual.attributes == (),
            actual.toString == "case toggle of True -> \"On\"; False -> \"Off\"",
            actual.isData == false
          )
        }
      )
    ),
    suite("Record")(
      suite("Attributed")(
        test("It should be possible to construct a record given attributes and fields") {
          val firstNameField = "firstName" -> string("John")
          val lastNameField  = "lastName"  -> string("Doe")
          val ageField       = "age"       -> int(21)
          val fields = Chunk(firstNameField, lastNameField, ageField).map { case (n, v) => Name.fromString(n) -> v }
          val recordFields =
            Chunk("firstName" -> stringType, "lastName" -> stringType, "age" -> intType).map(Type.field(_))
          val recordType = Type.record(recordFields)
          val actual     = record(recordType, firstNameField, lastNameField, ageField)
          assertTrue(
            actual == Record(recordType, firstNameField, lastNameField, ageField),
            actual == Record(recordType, fields),
            actual.attributes == recordType,
            actual.toString == "{firstName = \"John\", lastName = \"Doe\", age = 21}",
            actual.isData == true
          )
        }
      ),
      suite("Unattributed")(
        test("It should be possible to construct a record given attributes and fields") {
          val firstNameField = "firstName" -> string("John")
          val lastNameField  = "lastName"  -> string("Doe")
          val ageField       = "age"       -> int(21)
          val fields = Chunk(firstNameField, lastNameField, ageField).map { case (n, v) => Name.fromString(n) -> v }
          val actual = record(firstNameField, lastNameField, ageField)
          assertTrue(
            actual == Record.Raw(firstNameField, lastNameField, ageField),
            actual == Record.Raw(fields),
            actual.attributes == (),
            actual.toString == "{firstName = \"John\", lastName = \"Doe\", age = 21}",
            actual.isData == true
          )
        }
      )
    ),
    suite("Reference")(
      suite("Attributed")(
        test("It should be possible to construct given attributes and a FQ name as a string") {
          val fqName     = "Morphir:Morphir.SDK.Maybe:just"
          val attributes = "Maybe"
          val actual     = reference(attributes, fqName)
          assertTrue(
            actual == Reference(attributes, fqName),
            actual.attributes == "Maybe",
            actual.toString() == "Morphir.Morphir.SDK.Maybe.just",
            actual.isData == false
          )
        },
        test("It should be possible to construct given attributes and a FQName") {
          val fqName     = FQName.fqn("Morphir", "Morphir.SDK.Maybe", "just")
          val attributes = "Maybe"
          val actual     = reference(attributes, fqName)
          assertTrue(
            actual == Reference(attributes, fqName),
            actual.attributes == "Maybe",
            actual.toString() == "Morphir.Morphir.SDK.Maybe.just",
            actual.isData == false
          )
        }
      ),
      suite("Unattributed")(
        test("It should be possible to construct given a FQ name as a string") {

          val fqName = "Morphir:Morphir.SDK.Maybe:Just"
          val actual = reference(fqName)
          assertTrue(
            actual == Reference.Raw(fqName),
            actual == Reference((), fqName),
            actual.attributes == (),
            actual.toString() == "Morphir.Morphir.SDK.Maybe.just",
            actual.isData == false
          )
        },
        test("It should be possible to construct given attributes and a FQName") {
          val fqName = FQName.fqn("Morphir", "Morphir.SDK.Maybe", "Nothing")
          val actual = reference(fqName)
          assertTrue(
            actual == Reference.Raw(fqName),
            actual == Reference((), fqName),
            actual.attributes == (),
            actual.toString() == "Morphir.Morphir.SDK.Maybe.nothing",
            actual.isData == false
          )
        }
      )
    ),
    suite("Tuple")(
      suite("Attributed")(
        test("It should be possible to construct an empty tuple only given attributes") {
          val attributes = "EmptyTuple"
          val actual     = emptyTuple(attributes)
          assertTrue(
            actual == Tuple(attributes, Chunk.empty),
            actual == tuple(attributes, Chunk.empty),
            actual.attributes == "EmptyTuple",
            actual.toString() == "()",
            actual.isData == true
          )
        },
        test("It should be possible to construct a tuple given an attribute and a pair of elements") {
          val attributes = Type.tuple(stringType, intType)
          val element1   = string(stringType, "Scala")
          val element2   = int(intType, 3)
          val actual     = tuple(attributes, element1, element2)
          assertTrue(
            actual == Tuple(attributes, Chunk(element1, element2)),
            actual.attributes == attributes,
            actual.toString() == "(\"Scala\", 3)",
            actual.isData == true
          )
        },
        test("It should be possible to construct a tuple given an attribute and many elements") {
          val attributes = Type.tuple(stringType, intType, boolType)
          val element1   = string(stringType, "John Doe")
          val element2   = int(intType, 42)
          val element3   = boolean(boolType, true)
          val actual     = tuple(attributes, element1, element2, element3)
          assertTrue(
            actual == Tuple(attributes, Chunk(element1, element2, element3)),
            actual == tuple(attributes, Chunk(element1, element2, element3)),
            actual.attributes == attributes,
            actual.toString() == "(\"John Doe\", 42, True)",
            actual.isData == true
          )
        }
      ),
      suite("Unattributed")(
        test("It should be possible to construct an empty (un-attributed) tuple") {
          val actual = tuple()
          assertTrue(
            actual == Tuple.Raw(Chunk.empty),
            actual == Tuple((), Chunk.empty),
            actual == Tuple.Raw(),
            actual.attributes == (),
            actual.toString() == "()",
            actual.isData == true
          )
        },
        test("It should be possible to construct a (un-attributed) single element tuple") {
          val element = string("Hello")
          val actual  = tuple(element)
          assertTrue(
            actual == Tuple.Raw(Chunk(element)),
            actual == Tuple((), Chunk(element)),
            actual == Tuple.Raw(element),
            actual == Tuple.Raw(Chunk(element)),
            actual.attributes == (),
            actual.toString() == "(\"Hello\")",
            actual.isData == true
          )
        },
        test("It should be possible to construct a (un-attributed) pair of elements tuple") {
          val element1 = string("Hello")
          val element2 = int(42)
          val actual   = tuple(element1, element2)
          assertTrue(
            actual == Tuple.Raw(Chunk(element1, element2)),
            actual == Tuple((), Chunk(element1, element2)),
            actual == Tuple.Raw(element1, element2),
            actual == Tuple.Raw(Chunk(element1, element2)),
            actual.attributes == (),
            actual.toString() == "(\"Hello\", 42)",
            actual.isData == true
          )
        }
      )
    ),
    suite("Unit")(
      suite("Attributed")(
        test("It should support construction given attributes") {
          val actual = unit(Type.unit)
          assertTrue(
            actual.attributes == Type.unit,
            actual == Unit(Type.unit),
            actual.toString() == "()",
            actual.isData == true
          )
        }
      ),
      suite("Unattributed")(
        test("It should support construction given no attributes") {
          val actual = Value.unit
          assertTrue(
            actual.attributes == (),
            actual == Unit.Raw(),
            actual.toString() == "()",
            actual.isData == true
          )
        }
      )
    ),
    suite("UpdateRecord")(
      suite("Attributed")(
        test("It should support construction given attributes") {
          val accountType = Type.record(
            Type.Field(Name.fromString("accountNumber"), stringType),
            Type.Field(Name.fromString("balance"), floatType)
          )
          val attributes = accountType
          val account    = variable(accountType, "account")
          val actual     = update(accountType, account, "balance" -> float(floatType, 42000.00f))
          assertTrue(
            actual == UpdateRecord(
              attributes,
              account,
              Chunk(Name.fromString("balance") -> float(floatType, 42000.00f))
            ),
            actual.attributes == attributes,
            actual.toString() == "{ account | balance = 42000.0 }",
            actual.isData == false
          )
        }
      ),
      suite("Unattributed")(
        test("It should support construction given no attributes") {
          val account = variable("account")
          val actual  = update(account, "balance" -> float(floatType, 42000.00f))
          assertTrue(
            actual == UpdateRecord.Raw(
              account,
              Chunk(Name.fromString("balance") -> float(floatType, 42000.00f))
            ),
            actual.attributes == (),
            actual.toString() == "{ account | balance = 42000.0 }",
            actual.isData == false
          )
        }
      )
    ),
    suite("Variable")(
      suite("Attributed")(
        test("It should support construction given attributes and a name as a Sting") {
          val nameStr = "Alpha"
          val actual  = variable(stringType, nameStr)
          assertTrue(
            actual == Value(VariableCase(stringType, Name.fromString(nameStr))),
            actual.attributes == stringType,
            actual.toString == "alpha",
            actual == Variable(stringType, nameStr),
            actual match {
              case Variable(attributes, Name.VariableName("alpha")) if attributes == stringType => true
              case _                                                                            => false
            },
            actual.isData == false
          )
        },
        test("It should support construction given attributes and a name") {
          val name   = Name.fromString("Beta")
          val actual = variable(stringType, name)
          assertTrue(
            actual.attributes == stringType,
            actual.toString == "beta",
            actual == Value(VariableCase(stringType, name)),
            actual == Variable(stringType, name),
            actual.isData == false
          )
        }
      ),
      suite("Unattributed")(
        test("It should support construction from a string value") {
          val nameStr = "Gamma"
          val actual  = variable(nameStr)
          assertTrue(
            actual == Value(VariableCase((), Name.fromString(nameStr))),
            actual.attributes == (),
            actual.toString == "gamma",
            actual == Variable.Raw(nameStr),
            actual.isData == false
          )
        },
        test("It should support construction from a Name value") {
          val name   = Name.fromString("Epsilon")
          val actual = variable(name)
          assertTrue(
            actual == Value(VariableCase((), name)),
            actual.attributes == (),
            actual.toString == "epsilon",
            actual == Variable.Raw(name),
            actual.collectVariables == Set(name),
            actual.isData == false
          )
        },
        test("foldLeft should work as expected on a variable value") {
          val actual = Variable.Raw(Name.fromString("foo"))
          assertTrue(
            actual.foldLeft(Chunk.empty[RawValue])((acc, v) => v +: acc) == Chunk(actual)
          )
        }
      )
    )
  )
}
