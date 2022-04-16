package zio.morphir.ir

import zio.Chunk
import zio.morphir.ir.Type.Constructors
import zio.morphir.ir.Type.Type._
import zio.morphir.testing.MorphirBaseSpec
import zio.test._

object TypeModuleSpec extends MorphirBaseSpec {
  def spec: ZSpec[Environment, Any] = suite("Type")(
    suite("Operations")(
      test("Can be documented") {
        val actual = variable("a") ?? "Some type variable"
        assertTrue(actual.doc == "Some type variable")
      }
    ),
    suite("Variable")(
      test("testing first variable constructor") {
        val actual = variable("FizzBuzz")
        assertTrue(
          actual == Variable((), "FizzBuzz"),
          actual.collectVariables == Set(Name.fromString("FizzBuzz"))
        )
      },
      test("testing second variable constructor") {
        val actual = variable(Name("FizzBuzz"))
        assertTrue(
          actual == Variable((), "FizzBuzz"),
          actual.collectVariables == Set(Name.fromString("FizzBuzz"))
        )
      },
      test("eraseAttributes should clear out the Attributes") {
        val actual   = variable((0, 0), "foo")
        val expected = variable("foo")
        assertTrue(
          actual != expected,
          actual.attributes == ((0, 0)) && expected.attributes == (()),
          actual.eraseAttributes == variable("foo"),
          actual.eraseAttributes == actual.mapAttributes(_ => (()))
        )
      }
    ),
    suite("Field")(
      test("testing first field constructor") {
        val actual = field(Name("field1"), variable("FizzBuzz"))
        assertTrue(
          actual.name == Name("field1"),
          actual.fieldType == variable("FizzBuzz"),
          actual.fieldType.collectVariables == Set(Name.fromString("FizzBuzz"))
        )
      },
      test("testing second field constructor") {
        val actual = field("field1", variable("FizzBuzz"))
        assertTrue(
          actual.name == Name("field1"),
          actual.fieldType == variable("FizzBuzz"),
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
          actual == Record(Chunk(var1, var2))
        )
      },
      test("testing second record constructor") {
        val var1   = field("first", variable("hello"))
        val var2   = field("second", variable("there"))
        val actual = record(var1, var2)
        assertTrue(
          actual == Record(var1, var2)
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
          actual == Tuple.withElements(var1, var2),
          actual.attributes == ()
        )
      },
      test("testing second tuple constructor") {
        val var1   = variable("hello")
        val var2   = variable("there")
        val actual = tuple(var1, var2)
        assertTrue(
          actual == Tuple.withElements(var1, var2)
        )
      }
    ),
    suite("Function")(
      test("testing first function constructor") {
        val param1  = variable("v1")
        val retType = tuple(variable("v3"), variable("v4"))
        val actual  = function(param1, retType)
        assertTrue(
          actual == Function(param1, retType)
        )
      },
      test("testing second function constructor") {
        val param1  = variable("v1")
        val retType = tuple(variable("v3"), variable("v4"))
        val actual  = function("Hello", param1, retType)
        assertTrue(
          actual == Function("Hello", param1, retType)
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
          actual == ExtensibleRecord(n1, Chunk(f1, f2, f3)),
          actual == ExtensibleRecord("SomeName", f1, f2, f3)
        )
      },
      test("testing second extensible record constructor") {
        val f1     = field("first", variable("hello"))
        val f2     = field("second", variable("there"))
        val f3     = field("third", tuple(variable("v3"), variable("v4")))
        val n1     = Name("SomeName")
        val actual = extensibleRecordWithFields(n1, f1, f2, f3)
        assertTrue(
          actual == ExtensibleRecord(n1, Chunk(f1, f2, f3))
        )
      },
      test("testing third extensible record constructor") {
        val f1     = field("first", variable("hello"))
        val f2     = field("second", variable("there"))
        val f3     = field("third", tuple(variable("v3"), variable("v4")))
        val actual = extensibleRecord("SomeName", zio.Chunk(f1, f2, f3))
        assertTrue(
          actual == ExtensibleRecord(Name.fromString("SomeName"), Chunk(f1, f2, f3))
        )
      },
      test("testing fourth extensible record constructor") {
        val f1     = field("first", variable("hello"))
        val f2     = field("second", variable("there"))
        val f3     = field("third", tuple(variable("v3"), variable("v4")))
        val actual = extensibleRecordWithFields("SomeName", f1, f2, f3)
        assertTrue(
          actual == ExtensibleRecord(Name.fromString("SomeName"), Chunk(f1, f2, f3))
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
          actual == Reference(fqn1)(v1, v2, v3)
        )
      },
      test("testing second reference constructor") {
        val v1     = variable("v1")
        val v2     = variable("v2")
        val v3     = tuple(variable("v3"), variable("v4"))
        val fqn1   = FQName.fqn("packageName", "moduleName", "localName")
        val actual = reference(fqn1, v1, v2, v3)
        assertTrue(
          actual == Reference(fqn1)(v1, v2, v3)
        )
      },
      test("testing third reference constructor") {
        val v1     = variable("v1")
        val v2     = variable("v2")
        val v3     = tuple(variable("v3"), variable("v4"))
        val fqn1   = FQName.fqn("packageName", "moduleName", "localName")
        val actual = reference("packageName", "moduleName", "localName", zio.Chunk(v1, v2, v3))
        assertTrue(
          actual == Reference(fqn1)(v1, v2, v3)
        )
      },
      test("testing fourth reference constructor") {
        val v1     = variable("v1")
        val v2     = variable("v2")
        val v3     = tuple(variable("v3"), variable("v4"))
        val fqn1   = FQName.fqn("packageName", "moduleName", "localName")
        val actual = reference("packageName", "moduleName", "localName", v1, v2, v3)
        assertTrue(
          actual == Reference(fqn1)(v1, v2, v3)
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
