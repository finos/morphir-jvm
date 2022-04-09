package zio.morphir.ir.types.recursive

import zio.Chunk
import zio.morphir.ir.{FQName, Name}
import zio.morphir.testing.MorphirBaseSpec
import zio.test._

import TypeCase._
import Type._

object TypeExprSpec extends MorphirBaseSpec {
  def spec: ZSpec[Environment, Failure] = suite("TypeExpr Spec")(
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
          actual.satisfiesCaseOf { case RecordCase(_, fields) => fields.contains(var1) && fields.contains(var2) }
        )
      },
      test("testing unattributed record constructor given a list of fields") {
        val var1   = field("first", variable("hello"))
        val var2   = field("second", variable("there"))
        val actual = record(var1, var2)
        assertTrue(
          actual.satisfiesCaseOf { case RecordCase(_, fields) => fields.contains(var1) && fields.contains(var2) }
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
          actual.fieldCount == 3
        )
      }
    ),
    suite("Function")(
      test("testing first function constructor") {
        val param1  = variable("v1")
        val param2  = variable("v2")
        val retType = tuple(variable("v3"), variable("v4"))
        val actual  = function(Chunk(param1, param2), retType)
        assertTrue(
          actual.satisfiesCaseOf { case FunctionCase(_, params, returnType) =>
            params.contains(param1) && params.contains(param2) && returnType == retType
          }
        )
      },
      test("testing second function constructor") {
        val param1  = variable("v1")
        val param2  = variable("v2")
        val retType = tuple(variable("v3"), variable("v4"))
        val actual  = function(param1, param2)(retType)
        assertTrue(
          actual.satisfiesCaseOf { case FunctionCase(_, params, returnType) =>
            params.contains(param1) && params.contains(param2) && returnType == retType
          }
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
          }
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
      test("testing first reference constructor") {
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
          }
        )
      },
      test("testing second reference constructor") {
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
      test("testing third reference constructor") {
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
      test("testing fourth reference constructor") {
        val v1     = variable("v1")
        val v2     = variable("v2")
        val v3     = tuple(variable("v3"), variable("v4"))
        val fqn1   = FQName.fqn("packageName", "moduleName", "localName")
        val actual = reference("packageName", "moduleName", "localName", v1, v2, v3)
        assertTrue(
          actual.satisfiesCaseOf { case ReferenceCase(_, fqName, typeParams) =>
            fqName == fqn1 && typeParams.contains(v1) && typeParams.contains(v2) && typeParams.contains(v3)
          }
        )
      }
    ),
    suite("Tuple")(
      test("testing emptyTuple constructor") {
        val actual = emptyTuple("FizzBuzz")
        assertTrue(
          actual.satisfiesCaseOf { case TupleCase(attributes, fields) => fields.isEmpty && attributes == "FizzBuzz" },
          actual.attributes == "FizzBuzz",
          actual == Tuple("FizzBuzz")
        )
      },
      test("testing tuple constructor when given a chunk") {
        val var1   = variable("hello")
        val var2   = variable("there")
        val chunk  = zio.Chunk(var1, var2)
        val actual = tuple(chunk)
        assertTrue(
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
        val var2   = variable("there")
        val var3   = variable("notThere")
        val actual = tuple(var1, var2)
        assertTrue(
          actual.satisfiesCaseOf { case TupleCase(_, elements) =>
            elements.contains(var1) && elements.contains(var2) && !elements.contains(var3)
          },
          actual.attributes == ()
        )
      },
      test("testing tuple with attributes constructor") {
        val var1   = variable("A", "a")
        val var2   = variable("B", "b")
        val var3   = variable("C", "c")
        val actual = tuple("Tuple3[a,b,c]", var1, var2, var3)
        assertTrue(
          actual.attributes == "Tuple3[a,b,c]",
          actual.satisfiesCaseOf { case TupleCase(_, elements) => elements == Chunk(var1, var2, var3) }
        )
      }
    ),
    suite("Unit")(
      test("testing unattributed unit constructor") {
        val actual = Type.unit
        assertTrue(actual.attributes == ())
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
          }
        )

      }
    ),
    suite("Variable")(
      test("testing first variable constructor") {
        val actual = variable("FizzBuzz")
        assertTrue(actual.satisfiesCaseOf { case VariableCase(_, name) => name.toString == "[fizz,buzz]" }) &&
        assertTrue(actual.collectVariables == Set(Name.fromString("FizzBuzz"))) &&
        assertTrue(actual == Variable((), "FizzBuzz"))

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
          actual.eraseAttributes == actual.mapAttributes(_ => (()))
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
