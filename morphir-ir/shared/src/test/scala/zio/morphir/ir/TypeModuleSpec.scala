package zio.morphir.ir

import zio.morphir.testing.MorphirBaseSpec
import zio.morphir.ir.TypeModule.{Type, TypeCase}
import zio.test._
import zio.morphir.syntax.TypeModuleSyntax
import TypeCase._

object TypeModuleSpec extends MorphirBaseSpec with TypeModuleSyntax {
  def spec = suite("Type")(
    suite("Operations")(
      test("Can be documented") {
        val actual = variable("a") ?? "Some type variable"
        assertTrue(actual.doc == "Some type variable")
      }
    ),
    suite("Variable")(
      test("testing first variable constructor") {
        val actual = variable("FizzBuzz")
        assertTrue(actual.satisfiesCaseOf { case VariableCase(name) => name.toString == "[fizz,buzz]" }) &&
        assertTrue(actual.collectVariables == Set(Name.fromString("FizzBuzz")))
      },
      test("testing second variable constructor") {
        val actual = variable(Name("FizzBuzz"))
        assertTrue(actual.satisfiesCaseOf { case VariableCase(name) => name.toString == "[fizz,buzz]" }) &&
        assertTrue(actual.collectVariables == Set(Name.fromString("FizzBuzz")))
      },
      test("eraseAttributes should clear out the Attributes") {
        val actual   = variable("foo", (0, 0))
        val expected = variable("foo")
        assertTrue(
          actual != expected,
          actual.attributes == ((0, 0)) && expected.attributes == Type.emptyAttributes,
          actual.eraseAttributes == variable("foo"),
          actual.eraseAttributes == actual.mapAttributes(_ => Type.emptyAttributes)
        )
      }
    ),
    suite("Field")(
      test("testing first field constructor") {
        val actual = field(Name("field1"), variable("FizzBuzz"))
        assertTrue(
          actual.name == Name("field1"),
          actual.fieldType.satisfiesCaseOf { case VariableCase(name) => name.toString == "[fizz,buzz]" },
          actual.fieldType.collectVariables == Set(Name.fromString("FizzBuzz"))
        )
      },
      test("testing second field constructor") {
        val actual = field("field1", variable("FizzBuzz"))
        assertTrue(
          actual.name == Name("field1"),
          actual.fieldType.satisfiesCaseOf { case VariableCase(name) => name.toString == "[fizz,buzz]" },
          actual.fieldType.collectVariables == Set(Name.fromString("FizzBuzz"))
        )
      }
    ),
    suite("Record")(
      test("testing first record constructor") {
        val var1   = field("first", variable("hello"))
        val var2   = field("second", variable("there"))
        val chunk  = zio.Chunk(var1, var2)
        val actual = record(chunk)
        assertTrue(
          actual.satisfiesCaseOf { case RecordCase(fields) => fields.contains(var1) && fields.contains(var2) }
        )
      },
      test("testing second record constructor") {
        val var1   = field("first", variable("hello"))
        val var2   = field("second", variable("there"))
        val actual = record(var1, var2)
        assertTrue(
          actual.satisfiesCaseOf { case RecordCase(fields) => fields.contains(var1) && fields.contains(var2) }
        )
      }
    ),
    suite("Tuple")(
      test("testing first tuple constructor") {
        val var1   = variable("hello")
        val var2   = variable("there")
        val chunk  = zio.Chunk(var1, var2)
        val actual = tuple(chunk)
        assertTrue(
          actual.satisfiesCaseOf { case TupleCase(elements) => elements.contains(var1) && elements.contains(var2) }
        )
      },
      test("testing second tuple constructor") {
        val var1   = variable("hello")
        val var2   = variable("there")
        val var3   = variable("notThere")
        val actual = tuple(var1, var2)
        assertTrue(
          actual.satisfiesCaseOf { case TupleCase(elements) =>
            elements.contains(var1) && elements.contains(var2) && !elements.contains(var3)
          }
        )
      }
    ),
    suite("Function")(
      test("testing first function constructor") {
        val param1  = variable("v1")
        val param2  = variable("v2")
        val retType = tuple(variable("v3"), variable("v4"))
        val actual  = function(zio.Chunk(param1, param2), retType)
        assertTrue(
          actual.satisfiesCaseOf { case FunctionCase(params, returnType) =>
            params.contains(param1) && params.contains(param2) && returnType == retType
          }
        )
      },
      test("testing second function constructor") {
        val param1  = variable("v1")
        val param2  = variable("v2")
        val retType = tuple(variable("v3"), variable("v4"))
        val actual  = function(param1, param2)(retType, Type.emptyAttributes)
        assertTrue(
          actual.satisfiesCaseOf { case FunctionCase(params, returnType) =>
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
          actual.satisfiesCaseOf { case ExtensibleRecordCase(name, fields) =>
            name == n1 && fields.contains(f1) && fields.contains(f2) && fields.contains(f3)
          }
        )
      },
      test("testing second extensible record constructor") {
        val f1     = field("first", variable("hello"))
        val f2     = field("second", variable("there"))
        val f3     = field("third", tuple(variable("v3"), variable("v4")))
        val n1     = Name("SomeName")
        val actual = extensibleRecord(n1, f1, f2, f3)
        assertTrue(
          actual.satisfiesCaseOf { case ExtensibleRecordCase(name, fields) =>
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
          actual.satisfiesCaseOf { case ExtensibleRecordCase(name, fields) =>
            name.toString == "[some,name]" && fields.contains(f1) && fields.contains(f2) && fields.contains(f3)
          }
        )
      },
      test("testing fourth extensible record constructor") {
        val f1     = field("first", variable("hello"))
        val f2     = field("second", variable("there"))
        val f3     = field("third", tuple(variable("v3"), variable("v4")))
        val actual = extensibleRecord("SomeName", f1, f2, f3)
        assertTrue(
          actual.satisfiesCaseOf { case ExtensibleRecordCase(name, fields) =>
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
        val actual = reference(fqn1, zio.Chunk(v1, v2, v3))
        assertTrue(
          actual.satisfiesCaseOf { case ReferenceCase(fqName, typeParams) =>
            fqName == fqn1 && typeParams.contains(v1) && typeParams.contains(v2) && typeParams.contains(v3)
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
          actual.satisfiesCaseOf { case ReferenceCase(fqName, typeParams) =>
            fqName == fqn1 && typeParams.contains(v1) && typeParams.contains(v2) && typeParams.contains(v3)
          }
        )
      },
      test("testing third reference constructor") {
        val v1     = variable("v1")
        val v2     = variable("v2")
        val v3     = tuple(variable("v3"), variable("v4"))
        val fqn1   = FQName.fqn("packageName", "moduleName", "localName")
        val actual = reference("packageName", "moduleName", "localName", zio.Chunk(v1, v2, v3))
        assertTrue(
          actual.satisfiesCaseOf { case ReferenceCase(fqName, typeParams) =>
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
          actual.satisfiesCaseOf { case ReferenceCase(fqName, typeParams) =>
            fqName == fqn1 && typeParams.contains(v1) && typeParams.contains(v2) && typeParams.contains(v3)
          }
        )
      }
    ),
    suite("Constructors")(
      test("Can make type constructors for an enum") {
        val actual        = TypeConstructors.forEnum("Red", "Yellow", "Green")
        val expectedNames = Set("Red", "Yellow", "Green").map(Name.fromString)
        assertTrue(
          actual.ctorNames == expectedNames,
          actual.toMap.values.forall(_.isEmpty)
        )
      }
    )
  )
}
