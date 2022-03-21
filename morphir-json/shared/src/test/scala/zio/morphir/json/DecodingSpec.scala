package zio.morphir.json

import zio.json._
import zio.morphir.ir._
import zio.morphir.ir.TypeModule._
import zio.morphir.json.Decoders.MorphirJsonCodecV1._
import zio.test._
import zio.test.DefaultRunnableSpec

object DecodingSpec extends DefaultRunnableSpec {
  def spec = suite("decoding")(
    suite("Unit")(
      test("will decode a Unit") {
        val actual   = """[]"""
        val expected = ()
        assertTrue(actual.fromJson[Unit] == Right(expected))
      },
      test("will not decode a Unit") {
        val actual   = """["hello", "there"]"""
        val expected = Left("(Expected empty list, got [hello, there])")
        assertTrue(actual.fromJson[Unit] == expected)
      }
    ),
    suite("Name")(
      test("will decode an empty Name") {
        val actual   = "[]"
        val expected = Name.empty
        assertTrue(actual.fromJson[Name] == Right(expected))
      },
      test("will decode a single Name") {
        val actual   = """["hello"]"""
        val expected = Name("Hello")
        assertTrue(actual.fromJson[Name] == Right(expected))
      },
      test("will decode a Name") {
        val actual   = """["hello","there"]"""
        val expected = Name("HelloThere")
        assertTrue(actual.fromJson[Name] == Right(expected))
      },
      test("will decode a Name fromString") {
        val actual   = """["hello","there"]"""
        val expected = Name.fromString("Hello.There")
        assertTrue(actual.fromJson[Name] == Right(expected))
      },
      test("will decode a Name fromList") {
        val actual   = """["this","is","a","list"]"""
        val expected = Name.fromList(List("This", "is", "a", "list"))
        assertTrue(actual.fromJson[Name] == Right(expected))
      }
    ),
    suite("Path")(
      test("will decode an empty Path") {
        val actual   = "[]"
        val expected = Path.empty
        assertTrue(actual.fromJson[Path] == Right(expected))
      },
      test("will decode a simple Path") {
        val actual   = """[["org"]]"""
        val expected = Path.fromString("org")
        assertTrue(actual.fromJson[Path] == Right(expected))
      },
      test("will decode a Path") {
        val actual   = """[["org"],["foo"],["bar"]]"""
        val expected = Path.fromString("org.foo.bar")
        assertTrue(actual.fromJson[Path] == Right(expected))
      }
    ),
    suite("ModulePath")(
      test("will decode an empty ModulePath") {
        val actual   = "[]"
        val expected = ModulePath(Path.empty)
        assertTrue(actual.fromJson[ModulePath] == Right(expected))
      },
      test("will decode a simple ModulePath") {
        val actual   = """[["org"]]"""
        val expected = ModulePath(Path.fromString("org"))
        assertTrue(actual.fromJson[ModulePath] == Right(expected))
      },
      test("will decode a ModulePath") {
        val actual   = """[["org"],["foo"],["bar"]]"""
        val expected = ModulePath(Path.fromString("org.foo.bar"))
        assertTrue(actual.fromJson[ModulePath] == Right(expected))
      }
    ),
    suite("PackageName")(
      test("will decode an empty PackageName") {
        val actual   = "[]"
        val expected = PackageName(Path.empty)
        assertTrue(actual.fromJson[PackageName] == Right(expected))
      },
      test("will decode a simple PackageName") {
        val actual   = """[["org"]]"""
        val expected = PackageName(Path.fromString("org"))
        assertTrue(actual.fromJson[PackageName] == Right(expected))
      },
      test("will decode a PackageName") {
        val actual   = """[["org"],["foo"],["bar"]]"""
        val expected = PackageName(Path.fromString("org.foo.bar"))
        assertTrue(actual.fromJson[PackageName] == Right(expected))
      }
    ),
    suite("ModuleName")(
      test("will decode an empty ModuleName") {
        val actual   = "[[],[]]"
        val expected = ModuleModule.ModuleName(Path.empty, Name.empty)
        assertTrue(actual.fromJson[ModuleName] == Right(expected))
      },
      test("will decode a simple ModuleName") {
        val actual   = """[[["org"]],["src","test"]]"""
        val expected = ModuleModule.ModuleName(Path.fromString("org"), Name.fromString("SrcTest"))
        assertTrue(actual.fromJson[ModuleName] == Right(expected))
      },
      test("will decode a ModuleName") {
        val actual   = """[[["src"],["test"],["scala"]],["src","test"]]"""
        val expected = ModuleModule.ModuleName(Path.fromString("src.test.scala"), Name.fromString("SrcTest"))
        assertTrue(actual.fromJson[ModuleName] == Right(expected))
      }
    ),
    suite("QName")(
      test("will decode an empty QName") {
        val actual   = "[[],[]]"
        val expected = QName(Path.empty, Name.empty)
        assertTrue(actual.fromJson[QName] == Right(expected))
      },
      test("will decode a QName") {
        val actual   = """[[["proper"],["path"]],["name"]]"""
        val expected = QName.fromString("Proper.Path:name").get
        assertTrue(actual.fromJson[QName] == Right(expected))
      }
    ),
    suite("FQName")(
      test("will decode an empty FQName") {
        val actual   = "[[],[],[]]"
        val expected = FQName(Path.empty, Path.empty, Name.empty)
        assertTrue(actual.fromJson[FQName] == Right(expected))
      },
      test("will decode a FQName") {
        val actual   = """[[["com"],["example"]],[["java","home"]],["morphir"]]"""
        val expected = FQName.fromString("Com.Example;JavaHome;morphir", ";")
        assertTrue(actual.fromJson[FQName] == Right(expected))
      }
    ),
    suite("Documented")(
      test("will decode Documented for Integer") {
        val actual   = """["This is an Integer 10",10]"""
        val expected = Documented("This is an Integer 10", 10)
        assertTrue(actual.fromJson[Documented[Int]] == Right(expected))
      },
      test("will decode Documented for String") {
        val actual   = """["This is a String","Hello"]"""
        val expected = Documented("This is a String", "Hello")
        assertTrue(actual.fromJson[Documented[String]] == Right(expected))
      }
    ),
    suite("AccessControlled")(
      test("will decode AccessControlled for private Integer") {
        val actual   = """["private",10]"""
        val expected = AccessControlled(AccessControlled.Access.Private, 10)
        assertTrue(actual.fromJson[AccessControlled[Int]] == Right(expected))
      },
      test("will decode AccessControlled for public String") {
        val actual   = """["public","Hello"]"""
        val expected = AccessControlled(AccessControlled.Access.Public, "Hello")
        assertTrue(actual.fromJson[AccessControlled[String]] == Right(expected))
      }
    ),
    suite("Field")(
      test("will decode Field for private Integer") {
        val actual   = """[["name"],10]"""
        val expected = TypeModule.Field(Name.fromString("Name"), 10)
        assertTrue(actual.fromJson[Field[Int]] == Right(expected))
      },
      test("will decode Field for public String") {
        val actual = """[["string"],["public","Hello"]]"""
        val expected =
          TypeModule.Field(Name.fromString("String"), AccessControlled(AccessControlled.Access.Public, "Hello"))
        assertTrue(actual.fromJson[Field[AccessControlled[String]]] == Right(expected))
      }
    ),
    suite("Literal")(
      test("will decode a Literal.Bool") {
        val actual   = """["bool_literal",true]"""
        val expected = Literal.Bool(true)
        assertTrue(actual.fromJson[Literal.Bool] == Right(expected))
      },
      test("will decode a Literal.Char") {
        val actual   = """["char_literal","x"]"""
        val expected = Literal.Char('x')
        assertTrue(actual.fromJson[Literal.Char] == Right(expected))
      },
      test("will decode a Literal.Float") {
        val actual   = """["float_literal",1.3232]"""
        val expected = Literal.Float(new java.math.BigDecimal("1.3232"))
        assertTrue(actual.fromJson[Literal.Float] == Right(expected))
      },
      test("will decode a Literal.String") {
        val actual   = """["string_literal","hello"]"""
        val expected = Literal.String("hello")
        assertTrue(actual.fromJson[Literal.String] == Right(expected))
      },
      test("will decode an Literal.WholeNumber") {
        val actual   = """["int_literal",321321]"""
        val expected = Literal.WholeNumber(new java.math.BigInteger("321321"))
        assertTrue(actual.fromJson[Literal.WholeNumber] == Right(expected))
      }
    )
  )
}
