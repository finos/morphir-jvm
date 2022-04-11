package zio.morphir.ir.types.recursive

import zio.Chunk
import zio.morphir.ir.{FQName, Name, Source}
import zio.morphir.syntax.NamingSyntax
import zio.morphir.testing.MorphirBaseSpec
import zio.test._

import TypeCase._
import Type._

object RecursiveTypeSpec extends MorphirBaseSpec with NamingSyntax {
  def spec: ZSpec[Environment, Any] = suite("Type Spec")(
    suite("Operations")(
      test("Can be documented") {
        val actual = variable("a") ?? "Some type variable"
        assertTrue(actual.doc == "Some type variable")
      }
    ),
    suite("Field")(
      test("testing first field constructor") {
        val actual = field(Name("field1"), variable("FizzBuzz"))
        assertTrue(
          actual.name == Name("field1"),
          actual.fieldType.satisfiesCaseOf { case VariableCase(_, name) => name.toString == "[fizz,buzz]" },
          actual.fieldType.collectVariables == Set(Name.fromString("FizzBuzz"))
        )
      },
      test("testing second field constructor") {
        val actual = field("field1", variable("FizzBuzz"))
        assertTrue(
          actual.name == Name("field1"),
          actual.fieldType.satisfiesCaseOf { case VariableCase(_, name) => name.toString == "[fizz,buzz]" },
          actual.fieldType.collectVariables == Set(Name.fromString("FizzBuzz"))
        )
      }
    ),
    suite("Record")(
      test("testing unattributed record constructor given a chunk of fields") {
        val var1   = field("first", variable("hello"))
        val var2   = field("second", variable("there"))
        val chunk  = zio.Chunk(var1, var2)
        val actual = record(chunk)
        assertTrue(
          actual.fieldCount == 2,
          actual.satisfiesCaseOf { case RecordCase(_, fields) => fields.contains(var1) && fields.contains(var2) },
          actual.toString() == "{ first : hello, second : there }"
        )
      },
      test("testing unattributed record constructor given a list of fields") {
        val var1   = field("first", variable("hello"))
        val var2   = field("second", variable("there"))
        val actual = record(var1, var2)
        assertTrue(
          actual.satisfiesCaseOf { case RecordCase(_, fields) => fields.contains(var1) && fields.contains(var2) },
          actual.toString() == "{ first : hello, second : there }"
        )
      },
      test("testing unattributed record constructor given tuples representing fields") {
        val nameField   = ("name", reference("Morphir.SDK:Morphir.SDK.Basics:String"))
        val ageField    = ("age", reference("Morphir.SDK:Morphir.SDK.Basics:Int"))
        val salaryField = ("salary", reference("Morphir.SDK:Morphir.SDK.Basics:Double"))
        val actual      = record(nameField, ageField, salaryField)
        assertTrue(
          actual.attributes == (),
          actual == Record(field(nameField), field(ageField), field(salaryField)),
          actual.fieldCount == 3,
          actual.toString() == "{ name : Morphir.SDK.Morphir.SDK.Basics.String, age : Morphir.SDK.Morphir.SDK.Basics.Int, salary : Morphir.SDK.Morphir.SDK.Basics.Double }"
        )
      }
    ),
    suite("Function")(
      test("testing simple non-curried function") {
        val param      = variable("Input")
        val returnType = variable("Output")
        val actual     = function(param, returnType)
        assertTrue(
          actual == Function(param, returnType),
          actual.toString() == "input -> output"
        )
      },
      test("testing function with function argument") {
        val actual = function(function(variable("A"), variable("B")), variable("C"))
        assertTrue(
          actual == Function(Function(Variable((), "A"), Variable((), "B")), Variable((), "C")),
          actual.toString() == "(a -> b) -> c"
        )
      },
      test("testing function constructor(1)") {
        val param1  = variable("v1")
        val param2  = variable("v2")
        val retType = tuple(variable("v3"), variable("v4"))
        val actual  = function(param1, function(param2, retType))
        assertTrue(
          actual == Function(param1, Function(param2, retType)),
          actual.toString() == "v1 -> v2 -> (v3, v4)"
        )
      },
      test("testing function constructor(2)") {
        val param1  = variable("v1")
        val param2  = variable("v2")
        val retType = tuple(variable("v3"), variable("v4"))
        val actual  = function(param1, function(param2, retType))
        assertTrue(
          actual == Function(param1, Function(param2, retType))
        )
      }
    ),
    suite("Extensible Record")(
      test("testing first extensible record constructor") {
        val f1     = field("first", variable("hello"))
        val f2     = field("second", variable("there"))
        val f3     = field("third", tuple(variable("v3"), variable("v4")))
        val n1     = Name("SomeName")
        val actual = extensibleRecord(n1, zio.Chunk(f1, f2, f3))
        assertTrue(
          actual.satisfiesCaseOf { case ExtensibleRecordCase(_, name, fields) =>
            name == n1 && fields.contains(f1) && fields.contains(f2) && fields.contains(f3)
          }
        )
      },
      test("testing second extensible record constructor") {
        val f1     = field("first", variable("hello"))
        val f2     = field("second", variable("there"))
        val f3     = field("third", tuple(variable("v3"), variable("v4")))
        val n1     = Name("SomeName")
        val actual = extensibleRecordWithFields(n1, f1, f2, f3)
        assertTrue(
          actual.satisfiesCaseOf { case ExtensibleRecordCase(_, name, fields) =>
            name == n1 && fields.contains(f1) && fields.contains(f2) && fields.contains(f3)
          }
        )
      },
      test("testing third extensible record constructor") {

        val f1     = field("first", variable("hello"))
        val f2     = field("second", variable("there"))
        val f3     = field("third", tuple(variable("v3"), variable("v4")))
        val actual = extensibleRecord("SomeName", zio.Chunk(f1, f2, f3))
        assertTrue(
          actual.satisfiesCaseOf { case ExtensibleRecordCase(_, name, fields) =>
            name.toString == "[some,name]" && fields.contains(f1) && fields.contains(f2) && fields.contains(f3)
          },
          actual.toString() == "{ someName | first : hello, second : there, third : (v3, v4) }"
        )
      },
      test("testing fourth extensible record constructor") {
        val f1     = field("first", variable("hello"))
        val f2     = field("second", variable("there"))
        val f3     = field("third", tuple(variable("v3"), variable("v4")))
        val actual = extensibleRecordWithFields("SomeName", f1, f2, f3)
        assertTrue(
          actual.satisfiesCaseOf { case ExtensibleRecordCase(_, name, fields) =>
            name.toString == "[some,name]" && fields.contains(f1) && fields.contains(f2) && fields.contains(f3)
          }
        )
      }
    ),
    suite("Reference")(
      suite("Without Attributes")(
        test("testing construction given a FQName and Chunk of types") {
          val v1     = variable("v1")
          val v2     = variable("v2")
          val v3     = tuple(variable("v3"), variable("v4"))
          val fqn1   = FQName.fqn("packageName", "moduleName", "localName")
          val actual = reference(fqn1, Chunk(v1, v2, v3))
          assertTrue(
            actual == Reference(
              (),
              fqn1,
              Variable((), "v1"),
              Variable((), "v2"),
              Tuple((), Variable((), "v3"), Variable((), "v4"))
            ),
            actual.satisfiesCaseOf { case ReferenceCase(attributes, fqName, typeParams) =>
              attributes == () && fqName == fqn1 && typeParams.contains(v1) && typeParams.contains(v2) && typeParams
                .contains(v3)
            },
            actual.attributes == (),
            actual.toString() == "PackageName.ModuleName.LocalName v1 v2 (v3, v4)"
          )
        },
        test("testing construction given an FQName and a variadic list of types") {
          val v1     = variable("v1")
          val v2     = variable("v2")
          val v3     = tuple(variable("v3"), variable("v4"))
          val fqn1   = FQName.fqn("packageName", "moduleName", "localName")
          val actual = reference(fqn1, v1, v2, v3)
          assertTrue(
            actual.satisfiesCaseOf { case ReferenceCase(_, fqName, typeParams) =>
              fqName == fqn1 && typeParams.contains(v1) && typeParams.contains(v2) && typeParams.contains(v3)
            }
          )
        },
        test("testing construction given packageName, moduleName, localName and a Chunk of Types") {
          val v1     = variable("v1")
          val v2     = variable("v2")
          val v3     = tuple(variable("v3"), variable("v4"))
          val fqn1   = FQName.fqn("packageName", "moduleName", "localName")
          val actual = reference("packageName", "moduleName", "localName", Chunk(v1, v2, v3))
          assertTrue(
            actual.satisfiesCaseOf { case ReferenceCase(_, fqName, typeParams) =>
              fqName == fqn1 && typeParams.contains(v1) && typeParams.contains(v2) && typeParams.contains(v3)
            }
          )
        },
        test("testing given packageName, moduleName, localName and a variadic list of Types") {
          val v1     = variable("V1")
          val v2     = variable("V2")
          val v3     = tuple(variable("v3"), variable("v4"))
          val fqn1   = FQName.fqn("PackageName", "ModuleName", "LocalName")
          val actual = reference("PackageName", "ModuleName", "LocalName", v1, v2, v3)
          assertTrue(
            actual.satisfiesCaseOf { case ReferenceCase(_, fqName, typeParams) =>
              fqName == fqn1 && typeParams.contains(v1) && typeParams.contains(v2) && typeParams.contains(v3)
            },
            actual.toString == "PackageName.ModuleName.LocalName v1 v2 (v3, v4)"
          )
        }
      ),
      suite("With Attributes")(
        test("testing construction given attributes, FQName and Chunk no type parameters") {
          val refName = pkg("packageName") % "moduleName" % "localName"
          val actual  = reference(Source.Location.default, refName)
          assertTrue(
            actual.attributes == Source.Location.default,
            actual.collectReferences == Set(refName),
            actual == Reference(Source.Location.default, refName)
          )
        },
        test("testing construction given attributes, FQName, and a Chunk of types") {
          val v1 = variable(Source.Location.default, "V1")
          val v2 = variable(Source.Location.default.offsetRowBy(1), "V2")
          val v3 = tuple(
            Source.Location.default.offsetRowBy(4),
            variable(Source.Location.default.offsetRowBy(2), "v3"),
            variable(Source.Location.default.offsetRowBy(3), "v4")
          )
          val refName = FQName.fqn("PackageName", "ModuleName", "LocalName")
          val actual  = reference(Source.Location.default.offsetRowBy(6), refName, Chunk(v1, v2, v3))
          assertTrue(
            actual.toString == "PackageName.ModuleName.LocalName v1 v2 (v3, v4)",
            actual.attributes == Source.Location.default.offsetRowBy(6),
            actual.collectReferences == Set(refName),
            actual == Reference(Source.Location.default.offsetRowBy(6), refName, v1, v2, v3)
          )
        },
        test("testing given FQName and a variadic list of Types") {
          val v1     = variable(1, "V1")
          val v2     = variable(2, "V2")
          val v3     = tuple(3, variable(3, "v3"), variable(4, "v4"))
          val fqn    = FQName.fqn("PackageName", "ModuleName", "LocalName")
          val actual = reference(5, fqn, v1, v2, v3)
          assertTrue(
            actual.attributes == 5,
            actual.collectReferences == Set(fqn),
            actual == Reference(5, fqn, v1, v2, v3),
            actual.toString == "PackageName.ModuleName.LocalName v1 v2 (v3, v4)"
          )
        }
      )
    ),
    suite("Tuple")(
      test("testing emptyTuple constructor") {
        val actual = emptyTuple("FizzBuzz")
        assertTrue(
          actual.satisfiesCaseOf { case TupleCase(attributes, fields) => fields.isEmpty && attributes == "FizzBuzz" },
          actual.attributes == "FizzBuzz",
          actual == Tuple("FizzBuzz"),
          actual.toString() == "()"
        )
      },
      test("testing tuple constructor when given a chunk") {
        val var1   = variable("hello")
        val var2   = variable("there")
        val chunk  = zio.Chunk(var1, var2)
        val actual = tuple(chunk)
        assertTrue(
          actual.toString == "(hello, there)",
          actual.satisfiesCaseOf { case TupleCase(attributes, elements) =>
            attributes == () && elements.contains(var1) && elements.contains(var2)
          },
          actual == Tuple((), Variable((), "hello"), Variable((), "there")),
          actual match {
            case Tuple(attributes, Chunk(v1, v2)) => attributes == () && v1 == var1 && v2 == var2
            case _                                => false
          }
        )
      },
      test("testing tuple constructor when given multiple un-attributed elements") {
        val var1   = variable("hello")
        val var2   = variable("There")
        val var3   = variable("notThere")
        val actual = tuple(var1, var2)
        assertTrue(
          actual.satisfiesCaseOf { case TupleCase(_, elements) =>
            elements.contains(var1) && elements.contains(var2) && !elements.contains(var3)
          },
          actual.attributes == (),
          actual.toString() == "(hello, there)"
        )
      },
      test("testing tuple with attributes constructor") {
        val var1   = variable("A", "a")
        val var2   = variable("B", "b")
        val var3   = variable("C", "c")
        val actual = tuple("Tuple3[a,b,c]", var1, var2, var3)
        assertTrue(
          actual.attributes == "Tuple3[a,b,c]",
          actual.satisfiesCaseOf { case TupleCase(_, elements) => elements == Chunk(var1, var2, var3) },
          actual.toString() == "(a, b, c)"
        )
      }
    ),
    suite("Unit")(
      test("testing unattributed unit constructor") {
        val actual = Type.unit
        assertTrue(actual.attributes == (), actual.toString == "()")
      },
      test("testing attributed unit constructor") {
        val attributes = ("foo.scala", (0, 0), (5, 80))
        val actual     = unit(attributes)
        assertTrue(
          actual.attributes == attributes,
          actual == Unit(attributes),
          actual.satisfiesCaseOf { case UnitCase(actualAttributes) => actualAttributes == attributes },
          actual match {
            case Type.Unit(actualAttrbutes @ (_, _, _)) => actualAttrbutes == attributes
            case _                                      => false
          },
          actual.toString == "()"
        )

      }
    ),
    suite("Variable")(
      test("testing first variable constructor") {
        val actual = variable("FizzBuzz")
        assertTrue(actual.satisfiesCaseOf { case VariableCase(_, name) => name.toString == "[fizz,buzz]" }) &&
        assertTrue(actual.collectVariables == Set(Name.fromString("FizzBuzz"))) &&
        assertTrue(actual == Variable((), "FizzBuzz")) &&
        assertTrue(actual.toString == "fizzBuzz")

      },
      test("testing second variable constructor") {
        val actual = variable(Name("FizzBuzz"))
        assertTrue(actual.satisfiesCaseOf { case VariableCase(_, name) => name.toString == "[fizz,buzz]" }) &&
        assertTrue(actual.collectVariables == Set(Name.fromString("FizzBuzz"))) &&
        assertTrue(actual == Variable((), Name.fromString("FizzBuzz")))
      },
      test("eraseAttributes should clear out the Attributes") {
        val actual   = variable((0, 0), "foo")
        val expected = variable("foo")
        assertTrue(
          actual != expected,
          actual.attributes == ((0, 0)) && expected.attributes == (()),
          actual.eraseAttributes == UType.variable("foo"),
          actual.eraseAttributes == actual.mapAttributes(_ => (())),
          actual.toString == "foo"
        )
      }
    ),
    suite("Constructors")(
      test("Can make type constructors for an enum") {
        val actual        = Constructors.forEnum("Red", "Yellow", "Green")
        val expectedNames = Set("Red", "Yellow", "Green").map(Name.fromString)
        assertTrue(
          actual.ctorNames == expectedNames,
          actual.toMap.values.forall(_.isEmpty)
        )
      }
    )
  )
}
