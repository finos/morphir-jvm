package zio.morphir.json

import zio.json._
import zio.morphir.ir.Module.{
  Definition => ModuleDefinition,
  ModuleName,
  ModulePath,
  Specification => ModuleSpecification
}
import zio.morphir.ir.PackageModule.{Definition => PackageDefinition, Specification => PackageSpecification}
import zio.morphir.ir.Type.Type._
import zio.morphir.ir.Type.{Definition => TypeDefinition, Field, Specification => TypeSpecification, Type}
import zio.morphir.ir.Value.{Definition => ValueDefinition, Pattern, Specification => ValueSpecification, Value}
import zio.morphir.ir._
import zio.morphir.ir.value.recursive.ValueCase
import zio.morphir.json.MorphirJsonDecodingSupportV1._
import zio.test.{ZIOSpecDefault, ZSpec, _}

object DecodingSpecV1 extends ZIOSpecDefault {
  def spec: ZSpec[Environment, Any] = suite("Decoding Suite - V1")(
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
        val expected = ModuleName(Path.empty, Name.empty)
        assertTrue(actual.fromJson[ModuleName] == Right(expected))
      },
      test("will decode a simple ModuleName") {
        val actual   = """[[["org"]],["src","test"]]"""
        val expected = ModuleName(Path.fromString("org"), Name.fromString("SrcTest"))
        assertTrue(actual.fromJson[ModuleName] == Right(expected))
      },
      test("will decode a ModuleName") {
        val actual   = """[[["src"],["test"],["scala"]],["src","test"]]"""
        val expected = ModuleName(Path.fromString("src.test.scala"), Name.fromString("SrcTest"))
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
        val expected = Field(Name.fromString("Name"), 10)
        assertTrue(actual.fromJson[Field[Int]] == Right(expected))
      },
      test("will decode Field for public String") {
        val actual = """[["string"],["public","Hello"]]"""
        val expected =
          Field(Name.fromString("String"), AccessControlled(AccessControlled.Access.Public, "Hello"))
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
    ),
    suite("Type")(
      test("will decode Type.Unit") {
        val actual   = """["unit",1234]"""
        val expected = Type.Unit[Int](1234)
        assertTrue(
          actual.fromJson[Type[Int]] == Right(expected)
        )
      },
      test("will decode Type.Variable") {
        val actual   = """["variable",1234,["x"]]"""
        val expected = Type.variable[Int](1234, "x")
        assertTrue(
          actual.fromJson[Type[Int]] == Right(expected)
        )
      },
      test("will decode Type.Record") {
        val var1     = Field(Name("first"), variable[Int](123, "f"))
        val var2     = Field(Name("second"), variable[Int](345, "g"))
        val actual   = """["record",1,[[["first"],["variable",123,["f"]]],[["second"],["variable",345,["g"]]]]]"""
        val expected = Type.record(1, zio.Chunk(var1, var2))
        assertTrue(
          actual.fromJson[Type[Int]] == Right(expected)
        )
      },
      test("will decode Type.ExtensibleRecord") {
        val var1 = Field(Name("first"), variable[Int](123, "f"))
        val var2 = Field(Name("second"), variable[Int](345, "g"))
        val actual =
          """["extensible_record",1,["some","name"],[[["first"],["variable",123,["f"]]],[["second"],["variable",345,["g"]]]]]"""
        val expected = Type.ExtensibleRecord(1, Name.fromString("someName"), zio.Chunk(var1, var2))
        assertTrue(
          actual.fromJson[Type[Int]] == Right(expected)
        )
      },
      test("will decode Type.Tuple") {
        val var1     = variable[Int](123, "f")
        val var2     = variable[Int](345, "g")
        val actual   = """["tuple",1,[["variable",123,["f"]],["variable",345,["g"]]]]"""
        val expected = Type.Tuple(1, zio.Chunk(var1, var2))
        assertTrue(
          actual.fromJson[Type[Int]] == Right(expected)
        )
      },
      test("will decode Type.Reference") {
        val var1 = variable[Int](123, "f")
        val var2 = variable[Int](345, "g")
        val actual =
          """["reference",1,[[["test"]],[["java","home"]],["morphir"]],[["variable",123,["f"]],["variable",345,["g"]]]]"""
        val expected = Type.Reference(1, FQName.fromString("test:JavaHome:morphir"), zio.Chunk(var1, var2))
        assertTrue(
          actual.fromJson[Type[Int]] == Right(expected)
        )
      },
      test("will decode Type.Function") {
        val var1 = variable[Int](123, "f")
        val var2 = variable[Int](345, "g")
        val actual =
          """["function",1,["variable",123,["f"]],["variable",345,["g"]]]"""
        val expected = Type.function(1, var1, var2)
        assertTrue(
          actual.fromJson[Type[Int]] == Right(expected)
        )
      }
    ),
    suite("Constructors")(
      test("will decode empty Constructor") {
        val actual   = """[]"""
        val expected = zio.morphir.ir.Type.Constructors[Int](Map.empty)
        assertTrue(actual.fromJson[zio.morphir.ir.Type.Constructors[Int]] == Right(expected))
      },
      test("will decode Constructors with one constructor") {
        val name     = Name.fromString("name")
        val actual   = """[[["name"],[[["name"],["variable",123,["f"]]]]]]"""
        val expected = zio.morphir.ir.Type.Constructors[Int](Map((name, zio.Chunk((name, variable[Int](123, "f"))))))
        assertTrue(actual.fromJson[zio.morphir.ir.Type.Constructors[Int]] == Right(expected))
      },
      test("will decode Constructors") {
        val name1 = Name.fromString("name1")
        val name2 = Name.fromString("name2")
        val name3 = Name.fromString("name3")
        val name4 = Name.fromString("name4")
        val actual =
          """[[["name","1"],[[["name","1"],["variable",123,["f"]]],[["name","2"],["variable",345,["g"]]]]],[["name","2"],[[["name","3"],["variable",678,["h"]]],[["name","4"],["variable",789,["i"]]]]]]"""
        val expected = zio.morphir.ir.Type.Constructors[Int](
          Map(
            (name1, zio.Chunk((name1, variable[Int](123, "f")), (name2, variable[Int](345, "g")))),
            (name2, zio.Chunk((name3, variable[Int](678, "h")), (name4, variable[Int](789, "i"))))
          )
        )
        assertTrue(actual.fromJson[zio.morphir.ir.Type.Constructors[Int]] == Right(expected))
      }
    ),
    suite("TypeDefinition")(
      test("will decode TypeAlias") {
        val name1    = Name.fromString("name1")
        val name2    = Name.fromString("name2")
        val actual   = """["type_alias_definition",[["name","1"],["name","2"]],["variable",345,["g"]]]"""
        val expected = TypeDefinition.TypeAlias[Int](zio.Chunk(name1, name2), variable[Int](345, "g"))
        assertTrue(
          actual.fromJson[TypeDefinition.TypeAlias[Int]] == Right(expected),
          actual.fromJson[TypeDefinition[Int]] == Right(expected)
        )
      },
      test("will decode CustomType") {
        val name1 = Name.fromString("name1")
        val name2 = Name.fromString("name2")
        val name3 = Name.fromString("name3")
        val name4 = Name.fromString("name4")
        val ctors = AccessControlled(
          AccessControlled.Access.Public,
          zio.morphir.ir.Type.Constructors[Int](
            Map(
              (name1, zio.Chunk((name1, variable[Int](123, "f")), (name2, variable[Int](345, "g")))),
              (name2, zio.Chunk((name3, variable[Int](678, "h")), (name4, variable[Int](789, "i"))))
            )
          )
        )
        val actual =
          """["custom_type_definition",[["name","1"],["name","2"]],["public",[[["name","1"],[[["name","1"],["variable",123,["f"]]],[["name","2"],["variable",345,["g"]]]]],[["name","2"],[[["name","3"],["variable",678,["h"]]],[["name","4"],["variable",789,["i"]]]]]]]]"""
        val expected = TypeDefinition.CustomType[Int](zio.Chunk(name1, name2), ctors)
        assertTrue(
          actual.fromJson[TypeDefinition.CustomType[Int]] == Right(expected),
          actual.fromJson[TypeDefinition[Int]] == Right(expected)
        )
      }
    ),
    suite("TypeSpecification")(
      test("will decode TypeAliasSpecification") {
        val name1  = Name.fromString("name1")
        val name2  = Name.fromString("name2")
        val actual = """["type_alias_specification",[["name","1"],["name","2"]],["variable",345,["g"]]]"""
        val expected =
          TypeSpecification
            .TypeAliasSpecification[Int](zio.Chunk(name1, name2), variable[Int](345, "g"))
        assertTrue(
          actual.fromJson[TypeSpecification.TypeAliasSpecification[Int]] == Right(expected),
          actual.fromJson[TypeSpecification[Int]] == Right(expected)
        )
      },
      test("will decode CustomTypeSpecification") {
        val name1 = Name.fromString("name1")
        val name2 = Name.fromString("name2")
        val name3 = Name.fromString("name3")
        val name4 = Name.fromString("name4")
        val ctors = zio.morphir.ir.Type.Constructors[Int](
          Map(
            (name1, zio.Chunk((name1, variable[Int](123, "f")), (name2, variable[Int](345, "g")))),
            (name2, zio.Chunk((name3, variable[Int](678, "h")), (name4, variable[Int](789, "i"))))
          )
        )
        val actual =
          """["custom_type_specification",[["name","1"],["name","2"]],[[["name","1"],[[["name","1"],["variable",123,["f"]]],[["name","2"],["variable",345,["g"]]]]],[["name","2"],[[["name","3"],["variable",678,["h"]]],[["name","4"],["variable",789,["i"]]]]]]]"""
        val expected = TypeSpecification.CustomTypeSpecification[Int](zio.Chunk(name1, name2), ctors)
        assertTrue(
          actual.fromJson[TypeSpecification.CustomTypeSpecification[Int]] == Right(expected),
          actual.fromJson[TypeSpecification[Int]] == Right(expected)
        )
      },
      test("will decode OpaqueTypeSpecification") {
        val name1    = Name.fromString("name1")
        val name2    = Name.fromString("name2")
        val actual   = """["opaque_type_specification",[["name","1"],["name","2"]]]"""
        val expected = TypeSpecification.OpaqueTypeSpecification(zio.Chunk(name1, name2))
        assertTrue(
          actual.fromJson[TypeSpecification.OpaqueTypeSpecification] == Right(expected),
          actual.fromJson[TypeSpecification[Int]] == Right(expected)
        )
      }
    ),
    suite("ValueDefinition")(
      test("will decode ValueDefinition") {
        val inputParams = zio.Chunk(
          (Name.fromString("name1"), 1, variable[Int](345, "g")),
          (Name.fromString("name2"), 2, variable[Int](678, "h"))
        )
        val actual =
          """{"inputTypes":[[["name","1"],1,["variable",345,["g"]]],[["name","2"],2,["variable",678,["h"]]]],"outputType":["variable",345,["g"]],"body":["unit",1]}"""
        val expected =
          ValueDefinition[Int, Int](inputParams, variable[Int](345, "g"), Value[Int, Int](ValueCase.UnitCase(1)))
        assertTrue(actual.fromJson[ValueDefinition[Int, Int]] == Right(expected))
      }
    ),
    suite("ValueSpecification")(
      test("will decode ValueSpecification") {
        val inputs = zio.Chunk(
          (Name.fromString("name1"), variable[Int](345, "g")),
          (Name.fromString("name2"), variable[Int](678, "h"))
        )
        val actual =
          """{"inputs":[[["name","1"],["variable",345,["g"]]],[["name","2"],["variable",678,["h"]]]],"outputs":["variable",111,["f"]]}"""
        val expected = ValueSpecification[Int](inputs, variable[Int](111, "f"))
        assertTrue(actual.fromJson[ValueSpecification[Int]] == Right(expected))
      }
    ),
    suite("Pattern")(
      test("will decode AsPattern") {
        val actual   = """["as_pattern",1,["wildcard_pattern",1],["wild","card"]]"""
        val expected = Pattern.AsPattern[Int](Pattern.WildcardPattern[Int](1), Name.fromString("wildCard"), 1)
        assertTrue(
          actual.fromJson[Pattern.AsPattern[Int]] == Right(expected),
          actual.fromJson[Pattern[Int]] == Right(expected)
        )
      },
      test("will decode ConstructorPattern") {
        val patterns = zio.Chunk(
          Pattern.WildcardPattern[Int](1),
          Pattern.EmptyListPattern[Int](2),
          Pattern.AsPattern[Int](Pattern.WildcardPattern[Int](1), Name.fromString("wildCard"), 1)
        )
        val actual =
          """["constructor_pattern",1,[[["test"]],[["java","home"]],["morphir"]],[["wildcard_pattern",1],["empty_list_pattern",2],["as_pattern",1,["wildcard_pattern",1],["wild","card"]]]]"""
        val expected = Pattern.ConstructorPattern[Int](FQName.fromString("test:JavaHome:morphir"), patterns, 1)
        assertTrue(
          actual.fromJson[Pattern.ConstructorPattern[Int]] == Right(expected),
          actual.fromJson[Pattern[Int]] == Right(expected)
        )
      },
      test("will decode EmptyListPattern") {
        val actual   = """["empty_list_pattern",1]"""
        val expected = Pattern.EmptyListPattern[Int](1)
        assertTrue(
          actual.fromJson[Pattern.EmptyListPattern[Int]] == Right(expected),
          actual.fromJson[Pattern[Int]] == Right(expected)
        )
      },
      test("will decode LiteralPattern") {
        val actual   = """["literal_pattern",1,["string_literal","hello"]]"""
        val expected = Pattern.LiteralPattern[Any, Int](Literal.String("hello"), 1)
        assertTrue(actual.fromJson[Pattern.LiteralPattern[Any, Int]] == Right(expected))
      },
      test("will decode HeadTailPattern") {
        val actual = """["head_tail_pattern",1,["wildcard_pattern",1],["empty_list_pattern",2]]"""
        val expected =
          Pattern.HeadTailPattern[Int](Pattern.WildcardPattern[Int](1), Pattern.EmptyListPattern[Int](2), 1)
        assertTrue(
          actual.fromJson[Pattern.HeadTailPattern[Int]] == Right(expected),
          actual.fromJson[Pattern[Int]] == Right(expected)
        )
      },
      test("will decode TuplePattern") {
        val patterns = zio.Chunk(
          Pattern.WildcardPattern[Int](1),
          Pattern.UnitPattern[Int](2),
          Pattern.AsPattern[Int](Pattern.WildcardPattern[Int](1), Name.fromString("wildCard"), 1)
        )
        val actual =
          """["tuple_pattern",1,[["wildcard_pattern",1],["unit_pattern",2],["as_pattern",1,["wildcard_pattern",1],["wild","card"]]]]"""
        val expected = Pattern.TuplePattern[Int](patterns, 1)
        assertTrue(
          actual.fromJson[Pattern.TuplePattern[Int]] == Right(expected),
          actual.fromJson[Pattern[Int]] == Right(expected)
        )
      },
      test("will decode UnitPattern") {
        val actual   = """["unit_pattern",1]"""
        val expected = Pattern.UnitPattern[Int](1)
        assertTrue(
          actual.fromJson[Pattern.UnitPattern[Int]] == Right(expected),
          actual.fromJson[Pattern[Int]] == Right(expected)
        )
      },
      test("will decode WildcardPattern") {
        val actual   = """["wildcard_pattern",1]"""
        val expected = Pattern.WildcardPattern[Int](1)
        assertTrue(
          actual.fromJson[Pattern.WildcardPattern[Int]] == Right(expected),
          actual.fromJson[Pattern[Int]] == Right(expected)
        )
      }
    ),
    suite("ModuleSpecification")(
      test("will decode ModuleSpecification") {
        val name  = Name.fromString("name")
        val name1 = Name.fromString("name1")
        val name2 = Name.fromString("name2")

        val typeMap = Map(
          name -> Documented(
            "typeDoc1",
            TypeSpecification.TypeAliasSpecification[Int](zio.Chunk(name1, name2), variable[Int](345, "g"))
          )
        )
        val inputs = zio.Chunk((name1, variable[Int](345, "g")), (name2, variable[Int](678, "h")))
        val valueMap =
          Map(name -> Documented("valueDoc1", ValueSpecification[Int](inputs, variable[Int](111, "f"))))

        val actual =
          """{"types":[[["name"],["typeDoc1",["type_alias_specification",[["name","1"],["name","2"]],["variable",345,["g"]]]]]],"values":[[["name"],["valueDoc1",{"inputs":[[["name","1"],["variable",345,["g"]]],[["name","2"],["variable",678,["h"]]]],"outputs":["variable",111,["f"]]}]]]}"""
        val expected = ModuleSpecification[Int](typeMap, valueMap)
        assertTrue(actual.fromJson[ModuleSpecification[Int]] == Right(expected))
      }
    ),
    suite("PackageSpecification")(
      test("will decode PackageSpecification") {
        val name     = Name.fromString("name")
        val name1    = Name.fromString("name1")
        val name2    = Name.fromString("name2")
        val modName1 = ModuleName(Path.fromString("org"), Name.fromString("src"))
        val modName2 = ModuleName(Path.fromString("org"), Name.fromString("test"))

        val typeMap = Map(
          name -> Documented(
            "typeDoc1",
            TypeSpecification.TypeAliasSpecification[Int](zio.Chunk(name1, name2), variable[Int](345, "g"))
          )
        )
        val inputs = zio.Chunk((name1, variable[Int](345, "g")), (name2, variable[Int](678, "h")))
        val valueMap =
          Map(name -> Documented("valueDoc1", ValueSpecification[Int](inputs, variable[Int](111, "f"))))

        val modSpec  = ModuleSpecification[Int](typeMap, valueMap)
        val expected = PackageSpecification[Int](Map(modName1 -> modSpec, modName2 -> modSpec))
        val actual =
          """{"modules":[{"name":[[["org"]],["src"]],"spec":{"types":[[["name"],["typeDoc1",["type_alias_specification",[["name","1"],["name","2"]],["variable",345,["g"]]]]]],"values":[[["name"],["valueDoc1",{"inputs":[[["name","1"],["variable",345,["g"]]],[["name","2"],["variable",678,["h"]]]],"outputs":["variable",111,["f"]]}]]]}},{"name":[[["org"]],["test"]],"spec":{"types":[[["name"],["typeDoc1",["type_alias_specification",[["name","1"],["name","2"]],["variable",345,["g"]]]]]],"values":[[["name"],["valueDoc1",{"inputs":[[["name","1"],["variable",345,["g"]]],[["name","2"],["variable",678,["h"]]]],"outputs":["variable",111,["f"]]}]]]}}]}"""
        assertTrue(actual.fromJson[PackageSpecification[Int]] == Right(expected))
      }
    ),
    suite("ModuleDefinition")(
      test("will decode ModuleDefinition") {
        val name  = Name.fromString("name")
        val name1 = Name.fromString("name1")
        val name2 = Name.fromString("name2")
        val inputParams = zio.Chunk(
          (name1, 1, variable[Int](345, "g")),
          (name2, 2, variable[Int](678, "h"))
        )
        val value    = Value[Int, Int](ValueCase.ConstructorCase(1, FQName.fromString("test:JavaHome:morphir")))
        val valueDef = ValueDefinition[Int, Int](inputParams, variable[Int](345, "g"), value)

        val valueMap =
          Map(name -> AccessControlled(AccessControlled.Access.Private, Documented("valueDoc1", valueDef)))

        val typeMap = Map(
          name -> AccessControlled(
            AccessControlled.Access.Private,
            Documented(
              "typeDoc1",
              zio.morphir.ir.Type.Definition.TypeAlias[Int](zio.Chunk(name1, name2), variable[Int](345, "g"))
            )
          )
        )

        val expected = ModuleDefinition[Int, Int](typeMap, valueMap)
        val actual =
          """{"types":[[["name"],["private",["typeDoc1",["type_alias_definition",[["name","1"],["name","2"]],["variable",345,["g"]]]]]]],"values":[[["name"],["private",["valueDoc1",{"inputTypes":[[["name","1"],1,["variable",345,["g"]]],[["name","2"],2,["variable",678,["h"]]]],"outputType":["variable",345,["g"]],"body":["constructor",1,[[["test"]],[["java","home"]],["morphir"]]]}]]]]}"""
        assertTrue(actual.fromJson[ModuleDefinition[Int, Int]] == Right(expected))
      }
    ),
    suite("PackageDefinition")(
      test("will decode PackageDefinition") {
        val name     = Name.fromString("name")
        val name1    = Name.fromString("name1")
        val name2    = Name.fromString("name2")
        val modName1 = ModuleName(Path.fromString("org"), Name.fromString("src"))
        val modName2 = ModuleName(Path.fromString("org"), Name.fromString("test"))

        val inputParams = zio.Chunk(
          (name1, 1, variable[Int](345, "g")),
          (name2, 2, variable[Int](678, "h"))
        )
        val value =
          Value[Int, Int](ValueCase.ConstructorCase(1, FQName.fromString("test:JavaHome:morphir")))
        val valueDef = ValueDefinition[Int, Int](inputParams, variable[Int](345, "g"), value)

        val valueMap =
          Map(name -> AccessControlled(AccessControlled.Access.Private, Documented("valueDoc1", valueDef)))

        val typeMap = Map(
          name -> AccessControlled(
            AccessControlled.Access.Private,
            Documented(
              "typeDoc1",
              zio.morphir.ir.Type.Definition.TypeAlias[Int](zio.Chunk(name1, name2), variable[Int](345, "g"))
            )
          )
        )

        val modDef = ModuleDefinition[Int, Int](typeMap, valueMap)
        val actual =
          """{"modules":[{"name":[[["org"]],["src"]],"def":["public",{"types":[[["name"],["private",["typeDoc1",["type_alias_definition",[["name","1"],["name","2"]],["variable",345,["g"]]]]]]],"values":[[["name"],["private",["valueDoc1",{"inputTypes":[[["name","1"],1,["variable",345,["g"]]],[["name","2"],2,["variable",678,["h"]]]],"outputType":["variable",345,["g"]],"body":["constructor",1,[[["test"]],[["java","home"]],["morphir"]]]}]]]]}]},{"name":[[["org"]],["test"]],"def":["public",{"types":[[["name"],["private",["typeDoc1",["type_alias_definition",[["name","1"],["name","2"]],["variable",345,["g"]]]]]]],"values":[[["name"],["private",["valueDoc1",{"inputTypes":[[["name","1"],1,["variable",345,["g"]]],[["name","2"],2,["variable",678,["h"]]]],"outputType":["variable",345,["g"]],"body":["constructor",1,[[["test"]],[["java","home"]],["morphir"]]]}]]]]}]}]}"""
        val expected = PackageDefinition[Int, Int](
          Map(
            modName1 -> AccessControlled(AccessControlled.Access.Public, modDef),
            modName2 -> AccessControlled(AccessControlled.Access.Public, modDef)
          )
        )

        assertTrue(actual.fromJson[PackageDefinition[Int, Int]] == Right(expected))
      }
    ),
    suite("Value")(
      test("will decode Value - ApplyCase") {
        val unitCase = Value[Int, Int](ValueCase.UnitCase(6))
        val actual   = """["apply",3,["unit",6],["unit",6]]"""
        val expected = Value[Int, Int](ValueCase.ApplyCase(3, unitCase, unitCase))
        assertTrue(actual.fromJson[Value[Int, Int]] == Right(expected))
      },
      test("will decode Value - ConstructorCase") {
        val name     = FQName.fromString("Com.Example;JavaHome;morphir", ";")
        val actual   = """["constructor",3,[[["com"],["example"]],[["java","home"]],["morphir"]]]"""
        val expected = Value[Int, Int](ValueCase.ConstructorCase(3, name))
        assertTrue(actual.fromJson[Value[Int, Int]] == Right(expected))
      },
      test("will decode Value - DestructureCase") {
        val pattern  = Pattern.WildcardPattern[Int](1)
        val unitCase = Value[Int, Int](ValueCase.UnitCase(6))
        val actual   = """["destructure",3,["wildcard_pattern",1],["unit",6],["unit",6]]"""
        val expected = Value[Int, Int](ValueCase.DestructureCase(3, pattern, unitCase, unitCase))
        assertTrue(actual.fromJson[Value[Int, Int]] == Right(expected))
      },
      test("will decode Value - FieldCase") {
        val name     = Name("Hello")
        val unitCase = Value[Int, Int](ValueCase.UnitCase(6))
        val actual   = """["field",3,["unit",6],["hello"]]"""
        val expected = Value[Int, Int](ValueCase.FieldCase(3, unitCase, name))
        assertTrue(actual.fromJson[Value[Int, Int]] == Right(expected))
      },
      test("will decode Value - FieldFunctionCase") {
        val actual   = """["field_function",3,["hello"]]"""
        val expected = Value[Int, Int](ValueCase.FieldFunctionCase(3, Name("Hello")))
        assertTrue(actual.fromJson[Value[Int, Int]] == Right(expected))
      },
      test("will decode Value - IfThenElseCase") {
        val fieldFunctionCase = Value[Int, Int](ValueCase.FieldFunctionCase(3, Name("Hello")))
        val unitCase          = Value[Int, Int](ValueCase.UnitCase(6))
        val actual            = """["if_then_else",3,["unit",6],["field_function",3,["hello"]],["unit",6]]"""
        val expected          = Value[Int, Int](ValueCase.IfThenElseCase(3, unitCase, fieldFunctionCase, unitCase))
        assertTrue(actual.fromJson[Value[Int, Int]] == Right(expected))
      },
      test("will decode Value - LambdaCase") {
        val pattern           = Pattern.WildcardPattern[Int](1)
        val fieldFunctionCase = Value[Int, Int](ValueCase.FieldFunctionCase(3, Name("Hello")))
        val actual            = """["lambda",3,["wildcard_pattern",1],["field_function",3,["hello"]]]"""
        val expected          = Value[Int, Int](ValueCase.LambdaCase(3, pattern, fieldFunctionCase))
        assertTrue(actual.fromJson[Value[Int, Int]] == Right(expected))
      },
      // test("will decode Value - LetDefinitionCase") {
      //   val unitCase = Value[Int, Int](ValueCase.UnitCase(6))
      //   val fieldFunctionCase   = Value[Int, Int](ValueCase.FieldFunctionCase(3, Name("Hello")))
      //   val actual = """["list",3,[["unit",6],["field_function",3,["hello"]]]]"""
      //   val expected   = Value[Int, Int](ValueCase.LetDefinitionCase(3, Name("Hi"), ???, fieldFunctionCase))
      //   assertTrue(actual.fromJson[Value[Int, Int]] == Right(expected))
      // },
      // test("will decode Value - LetRecursionCase") {
      //   val unitCase = Value[Int, Int](ValueCase.UnitCase(6))
      //   val fieldFunctionCase   = Value[Int, Int](ValueCase.FieldFunctionCase(3, Name("Hello")))
      //   val actual = """["list",3,[["unit",6],["field_function",3,["hello"]]]]"""
      //   val expected   = Value[Int, Int](ValueCase.LetRecursionCase(3, ???, fieldFunctionCase))
      //   assertTrue(actual.fromJson[Value[Int, Int]] == Right(expected))
      // },
      test("will decode Value - ListCase") {
        val unitCase          = Value[Int, Int](ValueCase.UnitCase(6))
        val fieldFunctionCase = Value[Int, Int](ValueCase.FieldFunctionCase(3, Name("Hello")))
        val actual            = """["list",3,[["unit",6],["field_function",3,["hello"]]]]"""
        val expected = Value[Int, Int](ValueCase.ListCase(3, zio.Chunk[Value[Int, Int]](unitCase, fieldFunctionCase)))
        assertTrue(actual.fromJson[Value[Int, Int]] == Right(expected))
      },
      test("will decode Value - LiteralCase") {
        val literal  = Literal.Bool(true)
        val actual   = """["literal",3,["bool_literal",true]]"""
        val expected = Value[Int, Int](ValueCase.LiteralCase(3, literal))
        assertTrue(actual.fromJson[Value[Int, Int]] == Right(expected))
      },
      test("will decode Value - PatternMatchCase") {
        val unitCase          = Value[Int, Int](ValueCase.UnitCase(6))
        val fieldFunctionCase = Value[Int, Int](ValueCase.FieldFunctionCase(3, Name("Hello")))
        val patterns          = zio.Chunk((Pattern.WildcardPattern[Int](12), fieldFunctionCase))
        val actual   = """["pattern_match",3,["unit",6],[[["wildcard_pattern",12],["field_function",3,["hello"]]]]]"""
        val expected = Value[Int, Int](ValueCase.PatternMatchCase(3, unitCase, patterns))
        assertTrue(actual.fromJson[Value[Int, Int]] == Right(expected))
      },
      test("will decode Value - RecordCase") {
        val unitCase          = Value[Int, Int](ValueCase.UnitCase(6))
        val fieldFunctionCase = Value[Int, Int](ValueCase.FieldFunctionCase(3, Name("Hello")))
        val fields            = zio.Chunk((Name("hello"), fieldFunctionCase), (Name("there"), unitCase))
        val actual            = """["record",3,[[["hello"],["field_function",3,["hello"]]],[["there"],["unit",6]]]]"""
        val expected          = Value[Int, Int](ValueCase.RecordCase(3, fields))
        assertTrue(actual.fromJson[Value[Int, Int]] == Right(expected))
      },
      test("will decode Value - ReferenceCase") {
        val name     = FQName.fromString("Com.Example;JavaHome;morphir", ";")
        val actual   = """["reference",3,[[["com"],["example"]],[["java","home"]],["morphir"]]]"""
        val expected = Value[Int, Int](ValueCase.ReferenceCase(3, name))
        assertTrue(actual.fromJson[Value[Int, Int]] == Right(expected))
      },
      test("will decode Value - TupleCase") {
        val unitCase          = Value[Int, Int](ValueCase.UnitCase(6))
        val fieldFunctionCase = Value[Int, Int](ValueCase.FieldFunctionCase(3, Name("Hello")))
        val elements          = zio.Chunk(unitCase, fieldFunctionCase)
        val actual            = """["tuple",3,[["unit",6],["field_function",3,["hello"]]]]"""
        val expected          = Value[Int, Int](ValueCase.TupleCase(3, elements))
        assertTrue(actual.fromJson[Value[Int, Int]] == Right(expected))
      },
      test("will decode Value - UpdateRecordCase") {
        val unitCase          = Value[Int, Int](ValueCase.UnitCase(6))
        val fieldFunctionCase = Value[Int, Int](ValueCase.FieldFunctionCase(3, Name("Hello")))
        val fields            = zio.Chunk((Name("hello"), fieldFunctionCase), (Name("there"), unitCase))
        val actual =
          """["update_record",3,["unit",6],[[["hello"],["field_function",3,["hello"]]],[["there"],["unit",6]]]]"""
        val expected = Value[Int, Int](ValueCase.UpdateRecordCase(3, unitCase, fields))
        assertTrue(actual.fromJson[Value[Int, Int]] == Right(expected))
      },
      test("will decode Value - UnitCase") {
        val actual   = """["unit",6]"""
        val expected = Value[Int, Int](ValueCase.UnitCase(6))
        assertTrue(actual.fromJson[Value[Int, Int]] == Right(expected))
      },
      test("will decode Value - VariableCase") {
        val actual   = """["variable",3,["hello"]]"""
        val expected = Value[Int, Int](ValueCase.VariableCase(3, Name("hello")))
        assertTrue(actual.fromJson[Value[Int, Int]] == Right(expected))
      }
    )
  )
}
