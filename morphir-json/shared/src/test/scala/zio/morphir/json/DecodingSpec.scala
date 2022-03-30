package zio.morphir.json

import zio.json._
import zio.morphir.ir._
import zio.morphir.ir.TypeModule._
import zio.morphir.json.MorphirJsonDecodingSupportV1._
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
    ),
    suite("Type")(
      test("will decode Type.Unit") {
        val actual   = """["unit",1234]"""
        val expected = Type.Unit[Int](1234)
        assertTrue(
          actual.fromJson[Type.Unit[Int]] == Right(expected),
          actual.fromJson[Type[Int]] == Right(expected)
        )
      },
      test("will decode Type.Variable") {
        val actual   = """["variable",1234,["x"]]"""
        val expected = Type.variable[Int]("x", 1234)
        assertTrue(
          actual.fromJson[Type.Variable[Int]] == Right(expected),
          actual.fromJson[Type[Int]] == Right(expected)
        )
      },
      test("will decode Type.Record") {
        val var1     = Field(Name("first"), variable[Int]("f", 123))
        val var2     = Field(Name("second"), variable[Int]("g", 345))
        val actual   = """["record",1,[[["first"],["variable",123,["f"]]],[["second"],["variable",345,["g"]]]]]"""
        val expected = Type.Record(1, zio.Chunk(var1, var2))
        assertTrue(
          actual.fromJson[Type.Record[Int]] == Right(expected),
          actual.fromJson[Type[Int]] == Right(expected)
        )
      },
      test("will decode Type.ExtensibleRecord") {
        val var1 = Field(Name("first"), variable[Int]("f", 123))
        val var2 = Field(Name("second"), variable[Int]("g", 345))
        val actual =
          """["extensible_record",1,["some","name"],[[["first"],["variable",123,["f"]]],[["second"],["variable",345,["g"]]]]]"""
        val expected = Type.ExtensibleRecord(1, Name.fromString("someName"), zio.Chunk(var1, var2))
        assertTrue(
          actual.fromJson[Type.ExtensibleRecord[Int]] == Right(expected),
          actual.fromJson[Type[Int]] == Right(expected)
        )
      },
      test("will decode Type.Tuple") {
        val var1     = variable[Int]("f", 123)
        val var2     = variable[Int]("g", 345)
        val actual   = """["tuple",1,[["variable",123,["f"]],["variable",345,["g"]]]]"""
        val expected = Type.Tuple(1, zio.Chunk(var1, var2))
        assertTrue(
          actual.fromJson[Type.Tuple[Int]] == Right(expected),
          actual.fromJson[Type[Int]] == Right(expected)
        )
      },
      test("will decode Type.Reference") {
        val var1 = variable[Int]("f", 123)
        val var2 = variable[Int]("g", 345)
        val actual =
          """["reference",1,[[["test"]],[["java","home"]],["morphir"]],[["variable",123,["f"]],["variable",345,["g"]]]]"""
        val expected = Type.Reference(1, FQName.fromString("test:JavaHome:morphir"), zio.Chunk(var1, var2))
        assertTrue(
          actual.fromJson[Type.Reference[Int]] == Right(expected),
          actual.fromJson[Type[Int]] == Right(expected)
        )
      },
      test("will decode Type.Function") {
        val var1 = variable[Int]("f", 123)
        val var2 = variable[Int]("g", 345)
        val actual =
          """["function",1,[["variable",123,["f"]],["variable",345,["g"]]],["variable",345,["g"]]]"""
        val expected = Type.Function(1, zio.Chunk(var1, var2), var2)
        assertTrue(
          actual.fromJson[Type.Function[Int]] == Right(expected),
          actual.fromJson[Type[Int]] == Right(expected)
        )
      }
    ),
    suite("Constructors")(
      test("will decode empty Constructor") {
        val actual   = """[]"""
        val expected = TypeModule.Constructors[Int](Map.empty)
        assertTrue(actual.fromJson[TypeModule.Constructors[Int]] == Right(expected))
      },
      test("will decode Constructors with one constructor") {
        val name     = Name.fromString("name")
        val actual   = """[[["name"],[[["name"],["variable",123,["f"]]]]]]"""
        val expected = TypeModule.Constructors[Int](Map((name, zio.Chunk((name, variable[Int]("f", 123))))))
        assertTrue(actual.fromJson[TypeModule.Constructors[Int]] == Right(expected))
      },
      test("will decode Constructors") {
        val name1 = Name.fromString("name1")
        val name2 = Name.fromString("name2")
        val name3 = Name.fromString("name3")
        val name4 = Name.fromString("name4")
        val actual =
          """[[["name","1"],[[["name","1"],["variable",123,["f"]]],[["name","2"],["variable",345,["g"]]]]],[["name","2"],[[["name","3"],["variable",678,["h"]]],[["name","4"],["variable",789,["i"]]]]]]"""
        val expected = TypeModule.Constructors[Int](
          Map(
            (name1, zio.Chunk((name1, variable[Int]("f", 123)), (name2, variable[Int]("g", 345)))),
            (name2, zio.Chunk((name3, variable[Int]("h", 678)), (name4, variable[Int]("i", 789))))
          )
        )
        assertTrue(actual.fromJson[TypeModule.Constructors[Int]] == Right(expected))
      }
    ),
    suite("TypeModule.Definition")(
      test("will decode TypeAlias") {
        val name1    = Name.fromString("name1")
        val name2    = Name.fromString("name2")
        val actual   = """["type_alias_definition",[["name","1"],["name","2"]],["variable",345,["g"]]]"""
        val expected = TypeModule.Definition.TypeAlias[Int](zio.Chunk(name1, name2), variable[Int]("g", 345))
        assertTrue(
          actual.fromJson[TypeModule.Definition.TypeAlias[Int]] == Right(expected),
          actual.fromJson[TypeModule.Definition[Int]] == Right(expected)
        )
      },
      test("will decode CustomType") {
        val name1 = Name.fromString("name1")
        val name2 = Name.fromString("name2")
        val name3 = Name.fromString("name3")
        val name4 = Name.fromString("name4")
        val ctors = AccessControlled(
          AccessControlled.Access.Public,
          TypeModule.Constructors[Int](
            Map(
              (name1, zio.Chunk((name1, variable[Int]("f", 123)), (name2, variable[Int]("g", 345)))),
              (name2, zio.Chunk((name3, variable[Int]("h", 678)), (name4, variable[Int]("i", 789))))
            )
          )
        )
        val actual =
          """["custom_type_definition",[["name","1"],["name","2"]],["public",[[["name","1"],[[["name","1"],["variable",123,["f"]]],[["name","2"],["variable",345,["g"]]]]],[["name","2"],[[["name","3"],["variable",678,["h"]]],[["name","4"],["variable",789,["i"]]]]]]]]"""
        val expected = TypeModule.Definition.CustomType[Int](zio.Chunk(name1, name2), ctors)
        assertTrue(
          actual.fromJson[TypeModule.Definition.CustomType[Int]] == Right(expected),
          actual.fromJson[TypeModule.Definition[Int]] == Right(expected)
        )
      }
    ),
    suite("TypeModule.Specification")(
      test("will decode TypeAliasSpecification") {
        val name1  = Name.fromString("name1")
        val name2  = Name.fromString("name2")
        val actual = """["type_alias_specification",[["name","1"],["name","2"]],["variable",345,["g"]]]"""
        val expected =
          TypeModule.Specification.TypeAliasSpecification[Int](zio.Chunk(name1, name2), variable[Int]("g", 345))
        assertTrue(
          actual.fromJson[TypeModule.Specification.TypeAliasSpecification[Int]] == Right(expected),
          actual.fromJson[TypeModule.Specification[Int]] == Right(expected)
        )
      },
      test("will decode CustomTypeSpecification") {
        val name1 = Name.fromString("name1")
        val name2 = Name.fromString("name2")
        val name3 = Name.fromString("name3")
        val name4 = Name.fromString("name4")
        val ctors = TypeModule.Constructors[Int](
          Map(
            (name1, zio.Chunk((name1, variable[Int]("f", 123)), (name2, variable[Int]("g", 345)))),
            (name2, zio.Chunk((name3, variable[Int]("h", 678)), (name4, variable[Int]("i", 789))))
          )
        )
        val actual =
          """["custom_type_specification",[["name","1"],["name","2"]],[[["name","1"],[[["name","1"],["variable",123,["f"]]],[["name","2"],["variable",345,["g"]]]]],[["name","2"],[[["name","3"],["variable",678,["h"]]],[["name","4"],["variable",789,["i"]]]]]]]"""
        val expected = TypeModule.Specification.CustomTypeSpecification[Int](zio.Chunk(name1, name2), ctors)
        assertTrue(
          actual.fromJson[TypeModule.Specification.CustomTypeSpecification[Int]] == Right(expected),
          actual.fromJson[TypeModule.Specification[Int]] == Right(expected)
        )
      },
      test("will decode OpaqueTypeSpecification") {
        val name1    = Name.fromString("name1")
        val name2    = Name.fromString("name2")
        val actual   = """["opaque_type_specification",[["name","1"],["name","2"]]]"""
        val expected = TypeModule.Specification.OpaqueTypeSpecification(zio.Chunk(name1, name2))
        assertTrue(
          actual.fromJson[TypeModule.Specification.OpaqueTypeSpecification] == Right(expected),
          actual.fromJson[TypeModule.Specification[Int]] == Right(expected)
        )
      }
    ),
    suite("ValueModule.InputParameter")(
      test("will decode InputParameter") {
        val actual   = """[["name","1"],1,["variable",345,["g"]]]"""
        val expected = ValueModule.InputParameter[Int](Name.fromString("name1"), variable[Int]("g", 345), 1)
        assertTrue(actual.fromJson[ValueModule.InputParameter[Int]] == Right(expected))
      }
    ),
    suite("ValueModule.Definition")(
      test("will decode ValueModule.Definition") {
        val inputParams = zio.Chunk(
          ValueModule.InputParameter[Int](Name.fromString("name1"), variable[Int]("g", 345), 1),
          ValueModule.InputParameter[Int](Name.fromString("name2"), variable[Int]("h", 678), 2)
        )
        val actual =
          """{"inputTypes":[[["name","1"],1,["variable",345,["g"]]],[["name","2"],2,["variable",678,["h"]]]],"outputType":["variable",345,["g"]],"body":["wildcard_pattern",1]}"""
        val expected = ValueModule
          .Definition[Pattern[Int], Int](inputParams, variable[Int]("g", 345), Pattern.WildcardPattern[Int](1))
        assertTrue(actual.fromJson[ValueModule.Definition[Pattern[Int], Int]] == Right(expected))
      }
    ),
    suite("ValueModule.Specification")(
      test("will decode ValueModule.Specification") {
        val inputs = zio.Chunk(
          (Name.fromString("name1"), variable[Int]("g", 345)),
          (Name.fromString("name2"), variable[Int]("h", 678))
        )
        val actual =
          """{"inputs":[[["name","1"],["variable",345,["g"]]],[["name","2"],["variable",678,["h"]]]],"outputs":["variable",111,["f"]]}"""
        val expected = ValueModule.Specification[Int](inputs, variable[Int]("f", 111))
        assertTrue(actual.fromJson[ValueModule.Specification[Int]] == Right(expected))
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
      // test("will decode LiteralPattern") {
      //   val actual = """["literal_pattern",1,["string_literal","hello"]]"""
      //   val expected   = Pattern.LiteralPattern[String, Int](Literal.String("hello"), 1)
      //   assertTrue(actual.fromJson[Pattern.LiteralPattern[String,Int]] == Right(expected))
      // },
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
    suite("ModuleModule.Specification")(
      test("will decode ModuleModule.Specification") {
        val name  = Name.fromString("name")
        val name1 = Name.fromString("name1")
        val name2 = Name.fromString("name2")

        val typeMap = Map(
          name -> Documented(
            "typeDoc1",
            TypeModule.Specification.TypeAliasSpecification[Int](zio.Chunk(name1, name2), variable[Int]("g", 345))
          )
        )
        val inputs = zio.Chunk((name1, variable[Int]("g", 345)), (name2, variable[Int]("h", 678)))
        val valueMap =
          Map(name -> Documented("valueDoc1", ValueModule.Specification[Int](inputs, variable[Int]("f", 111))))

        val actual =
          """{"types":[[["name"],["typeDoc1",["type_alias_specification",[["name","1"],["name","2"]],["variable",345,["g"]]]]]],"values":[[["name"],["valueDoc1",{"inputs":[[["name","1"],["variable",345,["g"]]],[["name","2"],["variable",678,["h"]]]],"outputs":["variable",111,["f"]]}]]]}"""
        val expected = ModuleModule.Specification[Int](typeMap, valueMap)
        assertTrue(actual.fromJson[ModuleModule.Specification[Int]] == Right(expected))
      }
    ),
    suite("PackageModule.Specification")(
      test("will decode PackageModule.Specification") {
        val name     = Name.fromString("name")
        val name1    = Name.fromString("name1")
        val name2    = Name.fromString("name2")
        val modName1 = ModuleModule.ModuleName(Path.fromString("org"), Name.fromString("src"))
        val modName2 = ModuleModule.ModuleName(Path.fromString("org"), Name.fromString("test"))

        val typeMap = Map(
          name -> Documented(
            "typeDoc1",
            TypeModule.Specification.TypeAliasSpecification[Int](zio.Chunk(name1, name2), variable[Int]("g", 345))
          )
        )
        val inputs = zio.Chunk((name1, variable[Int]("g", 345)), (name2, variable[Int]("h", 678)))
        val valueMap =
          Map(name -> Documented("valueDoc1", ValueModule.Specification[Int](inputs, variable[Int]("f", 111))))

        val modSpec  = ModuleModule.Specification[Int](typeMap, valueMap)
        val expected = PackageModule.Specification[Int](Map(modName1 -> modSpec, modName2 -> modSpec))
        val actual =
          """{"modules":[{"name":[[["org"]],["src"]],"spec":{"types":[[["name"],["typeDoc1",["type_alias_specification",[["name","1"],["name","2"]],["variable",345,["g"]]]]]],"values":[[["name"],["valueDoc1",{"inputs":[[["name","1"],["variable",345,["g"]]],[["name","2"],["variable",678,["h"]]]],"outputs":["variable",111,["f"]]}]]]}},{"name":[[["org"]],["test"]],"spec":{"types":[[["name"],["typeDoc1",["type_alias_specification",[["name","1"],["name","2"]],["variable",345,["g"]]]]]],"values":[[["name"],["valueDoc1",{"inputs":[[["name","1"],["variable",345,["g"]]],[["name","2"],["variable",678,["h"]]]],"outputs":["variable",111,["f"]]}]]]}}]}"""
        assertTrue(actual.fromJson[PackageModule.Specification[Int]] == Right(expected))
      }
    ),
    suite("ModuleModule.Definition")(
      // test("will decode ModuleModule.Definition") {
      //   val name  = Name.fromString("name")
      //   val name1 = Name.fromString("name1")
      //   val name2 = Name.fromString("name2")
      //   val inputParams = zio.Chunk(
      //     ValueModule.InputParameter[Int](name1, variable[Int]("g", 345), 1),
      //     ValueModule.InputParameter[Int](name2, variable[Int]("h", 678), 2)
      //   )
      //   val value =
      //     ValueModule.Value[Int](ValueModule.ValueCase.ConstructorCase(FQName.fromString("test:JavaHome:morphir")), 1)
      //   val valueDef = ValueModule.Definition[ValueModule.Value[Int], Int](inputParams, variable[Int]("g", 345), value)

      //   val valueMap =
      //     Map(name -> AccessControlled(AccessControlled.Access.Private, Documented("valueDoc1", valueDef)))

      //   val typeMap = Map(
      //     name -> AccessControlled(
      //       AccessControlled.Access.Private,
      //       Documented(
      //         "typeDoc1",
      //         TypeModule.Definition.TypeAlias[Int](zio.Chunk(name1, name2), variable[Int]("g", 345))
      //       )
      //     )
      //   )

      //   val expected = ModuleModule.Definition[Int](typeMap, valueMap)
      //   val actual =
      //     """{"types":[[["name"],["private",["typeDoc1",["type_alias_definition",[["name","1"],["name","2"]],["variable",345,["g"]]]]]]],"values":[[["name"],["private",["valueDoc1",{"inputTypes":[[["name","1"],1,["variable",345,["g"]]],[["name","2"],2,["variable",678,["h"]]]],"outputType":["variable",345,["g"]],"body":["constructor",1,[[["test"]],[["java","home"]],["morphir"]]]}]]]]}"""
      //   // assertTrue(actual.fromJson[ModuleModule.Definition[Int]] == Right(expected))
      // }
    ),
    suite("PackageModule.Definition")(
      // test("will decode PackageModule.Definition") {
      //   val name     = Name.fromString("name")
      //   val name1    = Name.fromString("name1")
      //   val name2    = Name.fromString("name2")
      //   val modName1 = ModuleModule.ModuleName(Path.fromString("org"), Name.fromString("src"))
      //   val modName2 = ModuleModule.ModuleName(Path.fromString("org"), Name.fromString("test"))

      //   val inputParams = zio.Chunk(
      //     ValueModule.InputParameter[Int](name1, variable[Int]("g", 345), 1),
      //     ValueModule.InputParameter[Int](name2, variable[Int]("h", 678), 2)
      //   )
      //   val value =
      //     ValueModule.Value[Int](ValueModule.ValueCase.ConstructorCase(FQName.fromString("test:JavaHome:morphir")), 1)
      //   val valueDef = ValueModule.Definition[ValueModule.Value[Int], Int](inputParams, variable[Int]("g", 345), value)

      //   val valueMap =
      //     Map(name -> AccessControlled(AccessControlled.Access.Private, Documented("valueDoc1", valueDef)))

      //   val typeMap = Map(
      //     name -> AccessControlled(
      //       AccessControlled.Access.Private,
      //       Documented(
      //         "typeDoc1",
      //         TypeModule.Definition.TypeAlias[Int](zio.Chunk(name1, name2), variable[Int]("g", 345))
      //       )
      //     )
      //   )

      //   val modDef = ModuleModule.Definition[Int](typeMap, valueMap)
      //   val actual =
      //     """{"modules":[{"name":[[["org"]],["src"]],"def":["public",{"types":[[["name"],["private",["typeDoc1",["type_alias_definition",[["name","1"],["name","2"]],["variable",345,["g"]]]]]]],"values":[[["name"],["private",["valueDoc1",{"inputTypes":[[["name","1"],1,["variable",345,["g"]]],[["name","2"],2,["variable",678,["h"]]]],"outputType":["variable",345,["g"]],"body":["constructor",1,[[["test"]],[["java","home"]],["morphir"]]]}]]]]}]},{"name":[[["org"]],["test"]],"def":["public",{"types":[[["name"],["private",["typeDoc1",["type_alias_definition",[["name","1"],["name","2"]],["variable",345,["g"]]]]]]],"values":[[["name"],["private",["valueDoc1",{"inputTypes":[[["name","1"],1,["variable",345,["g"]]],[["name","2"],2,["variable",678,["h"]]]],"outputType":["variable",345,["g"]],"body":["constructor",1,[[["test"]],[["java","home"]],["morphir"]]]}]]]]}]}]}"""
      //   val expected = PackageModule.Definition[Int](
      //     Map(
      //       modName1 -> AccessControlled(AccessControlled.Access.Public, modDef),
      //       modName2 -> AccessControlled(AccessControlled.Access.Public, modDef)
      //     )
      //   )

      //   assertTrue(actual.fromJson[PackageModule.Definition[Int]] == Right(expected))
      // }
    )
  )
}
