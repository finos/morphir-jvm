package zio.morphir.json

import zio.json._
import zio.morphir.ir._
import zio.morphir.ir.TypeModule._
import zio.morphir.json.Encoders.MorphirJsonCodecV1._
import zio.test._
import zio.test.DefaultRunnableSpec
import zio.Chunk

object EncodingSpec extends DefaultRunnableSpec {
  def spec = suite("encoding")(
    suite("Unit")(
      test("will encode a Unit") {
        val actual   = ()
        val expected = "[]"
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("Name")(
      test("will encode an empty Name") {
        val actual   = Name.empty
        val expected = "[]"
        assertTrue(actual.toJson == expected)
      },
      test("will encode a single Name") {
        val actual   = Name("Hello")
        val expected = """["hello"]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a Name") {
        val actual   = Name("HelloThere")
        val expected = """["hello","there"]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a Name fromString") {
        val actual   = Name.fromString("Hello.There")
        val expected = """["hello","there"]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a Name fromList") {
        val actual   = Name.fromList(List("This", "is", "a", "list"))
        val expected = """["this","is","a","list"]"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("Path")(
      test("will encode an empty Path") {
        val actual   = Path.empty
        val expected = "[]"
        assertTrue(actual.toJson == expected)
      },
      test("will encode a simple Path") {
        val actual   = Path.fromString("org")
        val expected = """[["org"]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a Path") {
        val actual   = Path.fromString("org.foo.bar")
        val expected = """[["org"],["foo"],["bar"]]"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("ModulePath")(
      test("will encode an empty Path") {
        val actual   = ModulePath(Path.empty)
        val expected = "[]"
        assertTrue(actual.toJson == expected)
      },
      test("will encode a simple Path") {
        val actual   = ModulePath(Path.fromString("org"))
        val expected = """[["org"]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a Path") {
        val actual   = ModulePath(Path.fromString("org.foo.bar"))
        val expected = """[["org"],["foo"],["bar"]]"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("PackageName")(
      test("will encode an empty Path") {
        val actual   = PackageName(Path.empty)
        val expected = "[]"
        assertTrue(actual.toJson == expected)
      },
      test("will encode a simple Path") {
        val actual   = PackageName(Path.fromString("org"))
        val expected = """[["org"]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a Path") {
        val actual   = PackageName(Path.fromString("org.foo.bar"))
        val expected = """[["org"],["foo"],["bar"]]"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("ModuleName")(
      test("will encode an empty ModuleName") {
        val actual   = ModuleModule.ModuleName(Path.empty, Name.empty)
        val expected = "[[],[]]"
        assertTrue(actual.toJson == expected)
      },
      test("will encode a simple ModuleName") {
        val actual   = ModuleModule.ModuleName(Path.fromString("org"), Name.fromString("SrcTest"))
        val expected = """[[["org"]],["src","test"]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a ModuleName") {
        val actual   = ModuleModule.ModuleName(Path.fromString("src.test.scala"), Name.fromString("SrcTest"))
        val expected = """[[["src"],["test"],["scala"]],["src","test"]]"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("QName")(
      test("will encode an empty QName") {
        val actual   = QName(Path.empty, Name.empty)
        val expected = "[[],[]]"
        assertTrue(actual.toJson == expected)
      },
      test("will encode a QName") {
        val actual   = QName.fromString("Proper.Path:name")
        val expected = """[[["proper"],["path"]],["name"]]"""
        assertTrue(actual.get.toJson == expected)
      }
    ),
    suite("FQName")(
      test("will encode an empty FQName") {
        val actual   = FQName(Path.empty, Path.empty, Name.empty)
        val expected = "[[],[],[]]"
        assertTrue(actual.toJson == expected)
      },
      test("will encode a FQName") {
        val actual   = FQName.fromString("Com.Example;JavaHome;morphir", ";")
        val expected = """[[["com"],["example"]],[["java","home"]],["morphir"]]"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("Documented")(
      test("will encode Documented for Integer") {
        val actual   = Documented("This is an Integer 10", 10)
        val expected = """["This is an Integer 10",10]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode Documented for String") {
        val actual   = Documented("This is a String", "Hello")
        val expected = """["This is a String","Hello"]"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("AccessControlled")(
      test("will encode AccessControlled for private Integer") {
        val actual   = AccessControlled(AccessControlled.Access.Private, 10)
        val expected = """["private",10]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode AccessControlled for public String") {
        val actual   = AccessControlled(AccessControlled.Access.Public, "Hello")
        val expected = """["public","Hello"]"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("Field")(
      test("will encode Field for private Integer") {
        val actual   = TypeModule.Field(Name.fromString("Name"), AccessControlled(AccessControlled.Access.Private, 10))
        val expected = """[["name"],["private",10]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode Field for public String") {
        val actual =
          TypeModule.Field(Name.fromString("String"), AccessControlled(AccessControlled.Access.Public, "Hello"))
        val expected = """[["string"],["public","Hello"]]"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("Literal")(
      test("will encode a Literal.Bool") {
        val actual   = Literal.Bool(true)
        val expected = """["bool_literal",true]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a Literal.Char") {
        val actual   = Literal.Char('x')
        val expected = """["char_literal","x"]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a Literal.Float") {
        val actual   = Literal.Float(new java.math.BigDecimal("1.3232"))
        val expected = """["float_literal",1.3232]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a Literal.String") {
        val actual   = Literal.String("hello")
        val expected = """["string_literal","hello"]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode an Literal.WholeNumber") {
        val actual   = Literal.WholeNumber(new java.math.BigInteger("321321"))
        val expected = """["int_literal",321321]"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("Type")(
      test("will encode TypeCase.UnitCase") {
        val actual   = Type.unit[Int](1234)
        val expected = """["unit",1234]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode TypeCase.VariableCase") {
        val actual   = Type.variable[Int]("x", 1234)
        val expected = """["variable",1234,["x"]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode Field") {
        val actual   = Field(Name("someField"), Type.variable[Int]("x", 1234))
        val expected = """[["some","field"],["variable",1234,["x"]]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode TypeCase.RecordCase") {
        val var1: Field[Type[Int]]         = Field(Name("first"), variable[Int]("f", 123))
        val var2: Field[Type[Int]]         = Field(Name("second"), variable[Int]("g", 345))
        val chunk: Chunk[Field[Type[Int]]] = zio.Chunk(var1, var2)
        val actual                         = record(1, chunk)
        val expected = """["record",1,[[["first"],["variable",123,["f"]]],[["second"],["variable",345,["g"]]]]]"""
        assertTrue(actual.toJson == expected)
      }
    )
  )
}
