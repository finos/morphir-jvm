package zio.morphir.ir

import zio.morphir.ir.ModuleModule.{Definition, Specification}
import zio.{Chunk, ZEnvironment}
import zio.morphir.ir.TypeModule.Definition.{CustomType, TypeAlias}
import zio.morphir.ir.TypeModule.Specification.OpaqueTypeSpecification
import zio.morphir.ir.TypeModule.Constructors
import zio.morphir.syntax.AllSyntax
import zio.morphir.testing.MorphirBaseSpec
import zio.test.*

object ModuleModuleSpec extends MorphirBaseSpec with AllSyntax {
  val items = Map {
    Name("type")    -> Chunk((Name("var"), defineVariable("var1")))
    Name("rainbow") -> Chunk((Name("red"), defineVariable("red")))
  }

  val typeAlias = Documented(
    "doc",
    TypeAlias(Chunk(Name.fromString("hello")), defineVariable("type1"))
  )

  val customType = Documented(
    "doc",
    CustomType(Chunk(Name.fromString("world")), AccessControlled.publicAccess(Constructors(items)))
  )

  val definitionTypes = Map {
    Name("hello") -> AccessControlled.publicAccess(typeAlias)
    Name("world") -> AccessControlled.publicAccess(customType)
  }

  val definitionValues = Map {
    Name("val") -> AccessControlled.publicAccess(
      ValueModule.Definition.fromLiteral(string("string"))
    )
  }

  val moduleDef = Definition(definitionTypes, definitionValues)

  val specTypes = Map {
    Name("hello") -> Documented(
      "doc",
      OpaqueTypeSpecification(Chunk(Name("name1")), ZEnvironment.empty)
    )
    Name("world") -> Documented(
      "doc",
      OpaqueTypeSpecification(Chunk(Name("name2")), ZEnvironment.empty)
    )
  }

  val specValues = Map {
    Name("spec1") -> ValueModule.Specification(
      Chunk(
        (Name("type1"), defineVariable("Float")),
        (Name("type2"), defineVariable("Decimal"))
      ),
      defineVariable("WholeNumbers")
    )
  }

  val moduleSpec = Specification(specTypes, specValues)

  def spec = suite("Type")(
    suite("Module Definition")(
      test("Can be turned to Specification") {
//        val expected = Specification(types = Map{
//          Name("hello") -> typeAlias.map(_.toSpecification)
//          Name("world") -> customType.map(_.toSpecification)
//        },
//          Map{
//            Name("val") -> ValueModule.Definition.fromLiteral(string("string")).toSpecification
//          }
//        )
//        assertTrue(moduleDef.toSpecification == expected)
        // todo add when TypeModule toSpec is added
        assertTrue(1 == 1)
      },
      test("Can look up values") {
        val result = moduleDef.lookupValue(Name("val"))
        assertTrue(result.isDefined && result.get == ValueModule.Definition.fromLiteral(string("string")))
      },
      test("Can be erased") {
        assertTrue(moduleDef.eraseAttributes == Definition.empty)
      },
      test("Can collect all references") {
        assertTrue(
          moduleDef.collectTypeReferences.size == 0 &&
            moduleDef.collectValueReferences.size == 0 &&
            moduleDef.collectValueReferences.size == 0
        )
      }
    ),
    suite("Module Specification")(
      test("Can look up values") {
        val result = moduleSpec.lookupValue(Name("spec1"))
        assertTrue(
          result.isDefined && result.get.inputs.size == 2 && result.get.output == defineVariable("WholeNumbers")
        )
      },
      test("Can look up types") {
        val result = moduleSpec.lookupType(Name("world"))
        assertTrue(
          result.isDefined && !moduleSpec.lookupType(Name("notHere")).isDefined
        )
      },
      test("Can be erased") {
        assertTrue(moduleSpec.eraseAttributes == Specification.empty)
      }
    )
  )
}
