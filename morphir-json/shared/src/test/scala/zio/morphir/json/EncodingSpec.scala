package zio.morphir.json

// import zio.json._
// import zio.morphir.ir.Type.{Constructors, Definition, Field, Specification, Type}
// import zio.morphir.ir._
// import zio.morphir.ir.value.Pattern
// import zio.morphir.ir.Type.Type._
// import zio.morphir.json.MorphirJsonEncodingSupportV1._
// import zio.test._
import zio.test.DefaultRunnableSpec

object EncodingSpec extends DefaultRunnableSpec {
  def spec = suite("encoding")(
    suite("Unit")(
      // test("will encode a Unit") {
      //   val actual   = ()
      //   val expected = "[]"
      //   assertTrue(actual.toJson == expected)
      // }
    ),
    suite("Name")(
      // test("will encode an empty Name") {
      //   val actual   = Name.empty
      //   val expected = "[]"
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode a single Name") {
      //   val actual   = Name("Hello")
      //   val expected = """["hello"]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode a Name") {
      //   val actual   = Name("HelloThere")
      //   val expected = """["hello","there"]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode a Name fromString") {
      //   val actual   = Name.fromString("Hello.There")
      //   val expected = """["hello","there"]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode a Name fromList") {
      //   val actual   = Name.fromList(List("This", "is", "a", "list"))
      //   val expected = """["this","is","a","list"]"""
      //   assertTrue(actual.toJson == expected)
      // }
    ),
    suite("Path")(
      // test("will encode an empty Path") {
      //   val actual   = Path.empty
      //   val expected = "[]"
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode a simple Path") {
      //   val actual   = Path.fromString("org")
      //   val expected = """[["org"]]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode a Path") {
      //   val actual   = Path.fromString("org.foo.bar")
      //   val expected = """[["org"],["foo"],["bar"]]"""
      //   assertTrue(actual.toJson == expected)
      // }
    ),
    suite("ModulePath")(
      // test("will encode an empty Path") {
      //   val actual   = ModulePath(Path.empty)
      //   val expected = "[]"
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode a simple Path") {
      //   val actual   = ModulePath(Path.fromString("org"))
      //   val expected = """[["org"]]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode a Path") {
      //   val actual   = ModulePath(Path.fromString("org.foo.bar"))
      //   val expected = """[["org"],["foo"],["bar"]]"""
      //   assertTrue(actual.toJson == expected)
      // }
    ),
    suite("PackageName")(
      // test("will encode an empty Path") {
      //   val actual   = PackageName(Path.empty)
      //   val expected = "[]"
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode a simple Path") {
      //   val actual   = PackageName(Path.fromString("org"))
      //   val expected = """[["org"]]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode a Path") {
      //   val actual   = PackageName(Path.fromString("org.foo.bar"))
      //   val expected = """[["org"],["foo"],["bar"]]"""
      //   assertTrue(actual.toJson == expected)
      // }
    ),
    suite("ModuleName")(
      // test("will encode an empty ModuleName") {
      //   val actual   = ModuleModule.ModuleName(Path.empty, Name.empty)
      //   val expected = "[[],[]]"
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode a simple ModuleName") {
      //   val actual   = ModuleModule.ModuleName(Path.fromString("org"), Name.fromString("SrcTest"))
      //   val expected = """[[["org"]],["src","test"]]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode a ModuleName") {
      //   val actual   = ModuleModule.ModuleName(Path.fromString("src.test.scala"), Name.fromString("SrcTest"))
      //   val expected = """[[["src"],["test"],["scala"]],["src","test"]]"""
      //   assertTrue(actual.toJson == expected)
      // }
    ),
    suite("QName")(
      // test("will encode an empty QName") {
      //   val actual   = QName(Path.empty, Name.empty)
      //   val expected = "[[],[]]"
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode a QName") {
      //   val actual   = QName.fromString("Proper.Path:name")
      //   val expected = """[[["proper"],["path"]],["name"]]"""
      //   assertTrue(actual.get.toJson == expected)
      // }
    ),
    suite("FQName")(
      // test("will encode an empty FQName") {
      //   val actual   = FQName(Path.empty, Path.empty, Name.empty)
      //   val expected = "[[],[],[]]"
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode a FQName") {
      //   val actual   = FQName.fromString("Com.Example;JavaHome;morphir", ";")
      //   val expected = """[[["com"],["example"]],[["java","home"]],["morphir"]]"""
      //   assertTrue(actual.toJson == expected)
      // }
    ),
    suite("Documented")(
      // test("will encode Documented for Integer") {
      //   val actual   = Documented("This is an Integer 10", 10)
      //   val expected = """["This is an Integer 10",10]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode Documented for String") {
      //   val actual   = Documented("This is a String", "Hello")
      //   val expected = """["This is a String","Hello"]"""
      //   assertTrue(actual.toJson == expected)
      // }
    ),
    suite("AccessControlled")(
      // test("will encode AccessControlled for private Integer") {
      //   val actual   = AccessControlled(AccessControlled.Access.Private, 10)
      //   val expected = """["private",10]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode AccessControlled for public String") {
      //   val actual   = AccessControlled(AccessControlled.Access.Public, "Hello")
      //   val expected = """["public","Hello"]"""
      //   assertTrue(actual.toJson == expected)
      // }
    ),
    suite("Field")(
      // test("will encode Field for private Integer") {
      //   val actual   = Field(Name.fromString("Name"), AccessControlled(AccessControlled.Access.Private, 10))
      //   val expected = """[["name"],["private",10]]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode Field for public String") {
      //   val actual =
      //     Field(Name.fromString("String"), AccessControlled(AccessControlled.Access.Public, "Hello"))
      //   val expected = """[["string"],["public","Hello"]]"""
      //   assertTrue(actual.toJson == expected)
      // }
    ),
    suite("Literal")(
      // test("will encode a Literal.Bool") {
      //   val actual   = Literal.Bool(true)
      //   val expected = """["bool_literal",true]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode a Literal.Char") {
      //   val actual   = Literal.Char('x')
      //   val expected = """["char_literal","x"]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode a Literal.Float") {
      //   val actual   = Literal.Float(new java.math.BigDecimal("1.3232"))
      //   val expected = """["float_literal",1.3232]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode a Literal.String") {
      //   val actual   = Literal.String("hello")
      //   val expected = """["string_literal","hello"]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode an Literal.WholeNumber") {
      //   val actual   = Literal.WholeNumber(new java.math.BigInteger("321321"))
      //   val expected = """["int_literal",321321]"""
      //   assertTrue(actual.toJson == expected)
      // }
    ),
    suite("Type")(
      // test("will encode TypeCase.UnitCase") {
      //   val actual   = Type.unit[Int](1234)
      //   val expected = """["unit",1234]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode TypeCase.VariableCase") {
      //   val actual   = Type.variable[Int]("x", 1234)
      //   val expected = """["variable",1234,["x"]]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode Field") {
      //   val actual   = Field(Name("someField"), Type.variable[Int]("x", 1234))
      //   val expected = """[["some","field"],["variable",1234,["x"]]]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode TypeCase.RecordCase") {
      //   val var1     = Field(Name("first"), variable[Int]("f", 123))
      //   val var2     = Field(Name("second"), variable[Int]("g", 345))
      //   val actual   = record(1, zio.Chunk(var1, var2))
      //   val expected = """["record",1,[[["first"],["variable",123,["f"]]],[["second"],["variable",345,["g"]]]]]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode TypeCase.ExtensibleRecordCase") {
      //   val var1   = Field(Name("first"), variable[Int]("f", 123))
      //   val var2   = Field(Name("second"), variable[Int]("g", 345))
      //   val actual = extensibleRecord(1, Name.fromString("someName"), zio.Chunk(var1, var2))
      //   val expected =
      //     """["extensible_record",1,["some","name"],[[["first"],["variable",123,["f"]]],[["second"],["variable",345,["g"]]]]]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode TypeCase.TupleCase") {
      //   val var1     = variable[Int]("f", 123)
      //   val var2     = variable[Int]("g", 345)
      //   val actual   = tuple(1, var1, var2)
      //   val expected = """["tuple",1,[["variable",123,["f"]],["variable",345,["g"]]]]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode TypeCase.ReferenceCase") {
      //   val var1   = variable[Int]("f", 123)
      //   val var2   = variable[Int]("g", 345)
      //   val actual = reference(1, FQName.fromString("test:JavaHome:morphir"), zio.Chunk(var1, var2))
      //   val expected =
      //     """["reference",1,[[["test"]],[["java","home"]],["morphir"]],[["variable",123,["f"]],["variable",345,["g"]]]]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode TypeCase.FunctionCase") {
      //   val var1   = variable[Int]("f", 123)
      //   val var2   = variable[Int]("g", 345)
      //   val actual = function(1, zio.Chunk(var1, var2), var2)
      //   val expected =
      //     """["function",1,[["variable",123,["f"]],["variable",345,["g"]]],["variable",345,["g"]]]"""
      //   assertTrue(actual.toJson == expected)
      // }
    ),
    suite("Constructors")(
      // test("will encode empty Constructor") {
      //   val actual   = Constructors[Int](Map.empty)
      //   val expected = """[]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode Constructors with one constructor") {
      //   val name     = Name.fromString("name")
      //   val actual   = Constructors[Int](Map((name, zio.Chunk((name, variable[Int]("f", 123))))))
      //   val expected = """[[["name"],[[["name"],["variable",123,["f"]]]]]]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode Constructors") {
      //   val name1 = Name.fromString("name1")
      //   val name2 = Name.fromString("name2")
      //   val name3 = Name.fromString("name3")
      //   val name4 = Name.fromString("name4")
      //   val actual = Constructors[Int](
      //     Map(
      //       (name1, zio.Chunk((name1, variable[Int]("f", 123)), (name2, variable[Int]("g", 345)))),
      //       (name2, zio.Chunk((name3, variable[Int]("h", 678)), (name4, variable[Int]("i", 789))))
      //     )
      //   )
      //   val expected =
      //     """[[["name","1"],[[["name","1"],["variable",123,["f"]]],[["name","2"],["variable",345,["g"]]]]],[["name","2"],[[["name","3"],["variable",678,["h"]]],[["name","4"],["variable",789,["i"]]]]]]"""
      //   assertTrue(actual.toJson == expected)
      // }
    ),
    suite("zio.morphir.ir.Type..Definition")(
      // test("will encode TypeAlias") {
      //   val name1    = Name.fromString("name1")
      //   val name2    = Name.fromString("name2")
      //   val actual   = Definition.TypeAlias[Int](zio.Chunk(name1, name2), variable[Int]("g", 345))
      //   val expected = """["type_alias_definition",[["name","1"],["name","2"]],["variable",345,["g"]]]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode CustomType") {
      //   val name1 = Name.fromString("name1")
      //   val name2 = Name.fromString("name2")
      //   val name3 = Name.fromString("name3")
      //   val name4 = Name.fromString("name4")
      //   val ctors = AccessControlled(
      //     AccessControlled.Access.Public,
      //     Constructors[Int](
      //       Map(
      //         (name1, zio.Chunk((name1, variable[Int]("f", 123)), (name2, variable[Int]("g", 345)))),
      //         (name2, zio.Chunk((name3, variable[Int]("h", 678)), (name4, variable[Int]("i", 789))))
      //       )
      //     )
      //   )
      //   val actual = Definition.CustomType[Int](zio.Chunk(name1, name2), ctors)
      //   val expected =
      //     """["custom_type_definition",[["name","1"],["name","2"]],["public",[[["name","1"],[[["name","1"],["variable",123,["f"]]],[["name","2"],["variable",345,["g"]]]]],[["name","2"],[[["name","3"],["variable",678,["h"]]],[["name","4"],["variable",789,["i"]]]]]]]]"""
      //   assertTrue(actual.toJson == expected)
      // }
    ),
    suite("zio.morphir.ir.Type..Specification")(
      // test("will encode TypeAliasSpecification") {
      //   val name1 = Name.fromString("name1")
      //   val name2 = Name.fromString("name2")
      //   val actual =
      //     Specification.TypeAliasSpecification[Int](zio.Chunk(name1, name2), variable[Int]("g", 345))
      //   val expected = """["type_alias_specification",[["name","1"],["name","2"]],["variable",345,["g"]]]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode CustomTypeSpecification") {
      //   val name1 = Name.fromString("name1")
      //   val name2 = Name.fromString("name2")
      //   val name3 = Name.fromString("name3")
      //   val name4 = Name.fromString("name4")
      //   val ctors = Constructors[Int](
      //     Map(
      //       (name1, zio.Chunk((name1, variable[Int]("f", 123)), (name2, variable[Int]("g", 345)))),
      //       (name2, zio.Chunk((name3, variable[Int]("h", 678)), (name4, variable[Int]("i", 789))))
      //     )
      //   )
      //   val actual = Specification.CustomTypeSpecification[Int](zio.Chunk(name1, name2), ctors)
      //   val expected =
      //     """["custom_type_specification",[["name","1"],["name","2"]],[[["name","1"],[[["name","1"],["variable",123,["f"]]],[["name","2"],["variable",345,["g"]]]]],[["name","2"],[[["name","3"],["variable",678,["h"]]],[["name","4"],["variable",789,["i"]]]]]]]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode OpaqueTypeSpecification") {
      //   val name1  = Name.fromString("name1")
      //   val name2  = Name.fromString("name2")
      //   val actual = Specification.OpaqueTypeSpecification(zio.Chunk(name1, name2))
      //   val expected =
      //     """["opaque_type_specification",[["name","1"],["name","2"]]]"""
      //   assertTrue(actual.toJson == expected)
      // }
    ),
    suite("ValueModule.InputParameter")(
      // test("will encode InputParameter") {
      //   val actual   = ValueModule.InputParameter[Int](Name.fromString("name1"), variable[Int]("g", 345), 1)
      //   val expected = """[["name","1"],1,["variable",345,["g"]]]"""
      //   assertTrue(actual.toJson == expected)
      // }
    ),
    suite("Pattern")(
      // test("will encode AsPattern") {
      //   val actual   = Pattern.AsPattern[Int](Pattern.WildcardPattern[Int](1), Name.fromString("wildCard"), 1)
      //   val expected = """["as_pattern",1,["wildcard_pattern",1],["wild","card"]]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode ConstructorPattern") {
      //   val patterns = zio.Chunk(
      //     Pattern.WildcardPattern[Int](1),
      //     Pattern.EmptyListPattern[Int](2),
      //     Pattern.AsPattern[Int](Pattern.WildcardPattern[Int](1), Name.fromString("wildCard"), 1)
      //   )
      //   val actual = Pattern.ConstructorPattern[Int](FQName.fromString("test:JavaHome:morphir"), patterns, 1)
      //   val expected =
      //     """["constructor_pattern",1,[[["test"]],[["java","home"]],["morphir"]],[["wildcard_pattern",1],["empty_list_pattern",2],["as_pattern",1,["wildcard_pattern",1],["wild","card"]]]]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode EmptyListPattern") {
      //   val actual   = Pattern.EmptyListPattern[Int](1)
      //   val expected = """["empty_list_pattern",1]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode LiteralPattern") {
      //   val actual   = Pattern.LiteralPattern[String, Int](Literal.String("hello"), 1)
      //   val expected = """["literal_pattern",1,["string_literal","hello"]]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode HeadTailPattern") {
      //   val actual = Pattern.HeadTailPattern[Int](Pattern.WildcardPattern[Int](1), Pattern.EmptyListPattern[Int](2), 1)
      //   val expected = """["head_tail_pattern",1,["wildcard_pattern",1],["empty_list_pattern",2]]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode TuplePattern") {
      //   val patterns = zio.Chunk(
      //     Pattern.WildcardPattern[Int](1),
      //     Pattern.UnitPattern[Int](2),
      //     Pattern.AsPattern[Int](Pattern.WildcardPattern[Int](1), Name.fromString("wildCard"), 1)
      //   )
      //   val actual = Pattern.TuplePattern[Int](patterns, 1)
      //   val expected =
      //     """["tuple_pattern",1,[["wildcard_pattern",1],["unit_pattern",2],["as_pattern",1,["wildcard_pattern",1],["wild","card"]]]]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode UnitPattern") {
      //   val actual   = Pattern.UnitPattern[Int](1)
      //   val expected = """["unit_pattern",1]"""
      //   assertTrue(actual.toJson == expected)
      // },
      // test("will encode WildcardPattern") {
      //   val actual   = Pattern.WildcardPattern[Int](1)
      //   val expected = """["wildcard_pattern",1]"""
      //   assertTrue(actual.toJson == expected)
      // }
    ),
    suite("ValueModule.Definition")(
      // test("will encode ValueModule.Definition") {
      //   val inputParams = zio.Chunk(
      //     ValueModule.InputParameter[Int](Name.fromString("name1"), variable[Int]("g", 345), 1),
      //     ValueModule.InputParameter[Int](Name.fromString("name2"), variable[Int]("h", 678), 2)
      //   )
      //   val actual = ValueModule
      //     .Definition[Pattern[Int], Int](inputParams, variable[Int]("g", 345), Pattern.WildcardPattern[Int](1))
      //   val expected =
      //     """{"inputTypes":[[["name","1"],1,["variable",345,["g"]]],[["name","2"],2,["variable",678,["h"]]]],"outputType":["variable",345,["g"]],"body":["wildcard_pattern",1]}"""
      //   assertTrue(actual.toJson == expected)
      // }
    ),
    suite("ValueModule.Specification")(
      // test("will encode ValueModule.Specification") {
      //   val inputs = zio.Chunk(
      //     (Name.fromString("name1"), variable[Int]("g", 345)),
      //     (Name.fromString("name2"), variable[Int]("h", 678))
      //   )
      //   val actual = ValueModule.Specification[Int](inputs, variable[Int]("f", 111))
      //   val expected =
      //     """{"inputs":[[["name","1"],["variable",345,["g"]]],[["name","2"],["variable",678,["h"]]]],"outputs":["variable",111,["f"]]}"""
      //   assertTrue(actual.toJson == expected)
      // }
    ),
    suite("ModuleModule.Specification")(
      // test("will encode ModuleModule.Specification") {
      //   val name  = Name.fromString("name")
      //   val name1 = Name.fromString("name1")
      //   val name2 = Name.fromString("name2")

      //   val typeMap = Map(
      //     name -> Documented(
      //       "typeDoc1",
      //       zio.morphir.ir.Type.Specification
      //         .TypeAliasSpecification[Int](zio.Chunk(name1, name2), variable[Int]("g", 345))
      //     )
      //   )
      //   val inputs = zio.Chunk((name1, variable[Int]("g", 345)), (name2, variable[Int]("h", 678)))
      //   val valueMap =
      //     Map(name -> Documented("valueDoc1", ValueModule.Specification[Int](inputs, variable[Int]("f", 111))))

      //   val actual = ModuleModule.Specification[Int](typeMap, valueMap)
      //   val expected =
      //     """{"types":[[["name"],["typeDoc1",["type_alias_specification",[["name","1"],["name","2"]],["variable",345,["g"]]]]]],"values":[[["name"],["valueDoc1",{"inputs":[[["name","1"],["variable",345,["g"]]],[["name","2"],["variable",678,["h"]]]],"outputs":["variable",111,["f"]]}]]]}"""
      //   assertTrue(actual.toJson == expected)
      // }
    ),
    suite("ModuleModule.Definition")(
      // test("will encode ModuleModule.Definition") {
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
      //         zio.morphir.ir.Type.Definition.TypeAlias[Int](zio.Chunk(name1, name2), variable[Int]("g", 345))
      //       )
      //     )
      //   )

      //   val actual = ModuleModule.Definition[Int](typeMap, valueMap)
      //   val expected =
      //     """{"types":[[["name"],["private",["typeDoc1",["type_alias_definition",[["name","1"],["name","2"]],["variable",345,["g"]]]]]]],"values":[[["name"],["private",["valueDoc1",{"inputTypes":[[["name","1"],1,["variable",345,["g"]]],[["name","2"],2,["variable",678,["h"]]]],"outputType":["variable",345,["g"]],"body":["constructor",1,[[["test"]],[["java","home"]],["morphir"]]]}]]]]}"""
      //   assertTrue(actual.toJson == expected)
      // }
    ),
    suite("PackageModule.Specification")(
      // test("will encode PackageModule.Specification") {
      //   val name     = Name.fromString("name")
      //   val name1    = Name.fromString("name1")
      //   val name2    = Name.fromString("name2")
      //   val modName1 = ModuleModule.ModuleName(Path.fromString("org"), Name.fromString("src"))
      //   val modName2 = ModuleModule.ModuleName(Path.fromString("org"), Name.fromString("test"))

      //   val typeMap = Map(
      //     name -> Documented(
      //       "typeDoc1",
      //       zio.morphir.ir.Type.Specification
      //         .TypeAliasSpecification[Int](zio.Chunk(name1, name2), variable[Int]("g", 345))
      //     )
      //   )
      //   val inputs = zio.Chunk((name1, variable[Int]("g", 345)), (name2, variable[Int]("h", 678)))
      //   val valueMap =
      //     Map(name -> Documented("valueDoc1", ValueModule.Specification[Int](inputs, variable[Int]("f", 111))))

      //   val modSpec = ModuleModule.Specification[Int](typeMap, valueMap)
      //   val actual  = PackageModule.Specification[Int](Map(modName1 -> modSpec, modName2 -> modSpec))
      //   val expected =
      //     """{"modules":[{"name":[[["org"]],["src"]],"spec":{"types":[[["name"],["typeDoc1",["type_alias_specification",[["name","1"],["name","2"]],["variable",345,["g"]]]]]],"values":[[["name"],["valueDoc1",{"inputs":[[["name","1"],["variable",345,["g"]]],[["name","2"],["variable",678,["h"]]]],"outputs":["variable",111,["f"]]}]]]}},{"name":[[["org"]],["test"]],"spec":{"types":[[["name"],["typeDoc1",["type_alias_specification",[["name","1"],["name","2"]],["variable",345,["g"]]]]]],"values":[[["name"],["valueDoc1",{"inputs":[[["name","1"],["variable",345,["g"]]],[["name","2"],["variable",678,["h"]]]],"outputs":["variable",111,["f"]]}]]]}}]}"""
      //   assertTrue(actual.toJson == expected)
      // }
    ),
    suite("PackageModule.Definition")(
      //   test("will encode PackageModule.Definition") {
      //     val name     = Name.fromString("name")
      //     val name1    = Name.fromString("name1")
      //     val name2    = Name.fromString("name2")
      //     val modName1 = ModuleModule.ModuleName(Path.fromString("org"), Name.fromString("src"))
      //     val modName2 = ModuleModule.ModuleName(Path.fromString("org"), Name.fromString("test"))

      //     val inputParams = zio.Chunk(
      //       ValueModule.InputParameter[Int](name1, variable[Int]("g", 345), 1),
      //       ValueModule.InputParameter[Int](name2, variable[Int]("h", 678), 2)
      //     )
      //     val value =
      //       ValueModule.Value[Int](ValueModule.ValueCase.ConstructorCase(FQName.fromString("test:JavaHome:morphir")), 1)
      //     val valueDef = ValueModule.Definition[ValueModule.Value[Int], Int](inputParams, variable[Int]("g", 345), value)

      //     val valueMap =
      //       Map(name -> AccessControlled(AccessControlled.Access.Private, Documented("valueDoc1", valueDef)))

      //     val typeMap = Map(
      //       name -> AccessControlled(
      //         AccessControlled.Access.Private,
      //         Documented(
      //           "typeDoc1",
      //           zio.morphir.ir.Type.Definition.TypeAlias[Int](zio.Chunk(name1, name2), variable[Int]("g", 345))
      //         )
      //       )
      //     )

      //     val modDef = ModuleModule.Definition[Int](typeMap, valueMap)
      //     val actual = PackageModule.Definition[Int](
      //       Map(
      //         modName1 -> AccessControlled(AccessControlled.Access.Public, modDef),
      //         modName2 -> AccessControlled(AccessControlled.Access.Public, modDef)
      //       )
      //     )

      //     val expected =
      //       """{"modules":[{"name":[[["org"]],["src"]],"def":["public",{"types":[[["name"],["private",["typeDoc1",["type_alias_definition",[["name","1"],["name","2"]],["variable",345,["g"]]]]]]],"values":[[["name"],["private",["valueDoc1",{"inputTypes":[[["name","1"],1,["variable",345,["g"]]],[["name","2"],2,["variable",678,["h"]]]],"outputType":["variable",345,["g"]],"body":["constructor",1,[[["test"]],[["java","home"]],["morphir"]]]}]]]]}]},{"name":[[["org"]],["test"]],"def":["public",{"types":[[["name"],["private",["typeDoc1",["type_alias_definition",[["name","1"],["name","2"]],["variable",345,["g"]]]]]]],"values":[[["name"],["private",["valueDoc1",{"inputTypes":[[["name","1"],1,["variable",345,["g"]]],[["name","2"],2,["variable",678,["h"]]]],"outputType":["variable",345,["g"]],"body":["constructor",1,[[["test"]],[["java","home"]],["morphir"]]]}]]]]}]}]}"""
      //     assertTrue(actual.toJson == expected)
      //   }
    )
  )
}
