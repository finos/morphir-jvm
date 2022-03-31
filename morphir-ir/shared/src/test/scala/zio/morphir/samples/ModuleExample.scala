package zio.morphir.samples

import zio.Chunk
import zio.morphir.ir.ModuleModule.{Definition, Specification}
import zio.morphir.ir.ModuleModuleSpec.{defineVariable, string}
import zio.morphir.ir.Type.Constructors
import zio.morphir.ir.Type.Definition.{CustomType, TypeAlias}
import zio.morphir.ir.Type.Specification.OpaqueTypeSpecification
import zio.morphir.ir.{AccessControlled, Documented, Name, ValueModule}
import zio.morphir.ir.types.UType

object ModuleExample {
  val items: Map[Name, Chunk[(Name, UType)]] = Map {
    Name("type")    -> Chunk((Name("var"), defineVariable("var1")))
    Name("rainbow") -> Chunk((Name("red"), defineVariable("red")))
  }

  val typeAlias = Documented(
    "doc",
    TypeAlias(Chunk(Name.fromString("hello")), defineVariable("type1"))
  )

  val customType = Documented(
    "doc",
    CustomType[Any](Chunk(Name.fromString("world")), AccessControlled.publicAccess(Constructors(items)))
  )

  val definitionTypes = Map {
    Name("hello") -> AccessControlled.publicAccess(typeAlias)
    Name("world") -> AccessControlled.publicAccess(customType)
  }

  val definitionValues = Map {
    Name("val") -> AccessControlled.publicAccess(
      Documented("type", ValueModule.Definition.fromLiteral(string("string")))
    )
  }

  val moduleDef = Definition(definitionTypes, definitionValues)

  val specTypes = Map {
    Name("hello") -> Documented(
      "doc",
      OpaqueTypeSpecification(Chunk(Name("name1")))
    )
    Name("world") -> Documented(
      "doc",
      OpaqueTypeSpecification(Chunk(Name("name2")))
    )
  }

  val specValues = Map {
    Name("spec1") -> Documented(
      "types",
      ValueModule.Specification(
        Chunk(
          (Name("type1"), defineVariable("Float")),
          (Name("type2"), defineVariable("Decimal"))
        ),
        defineVariable("WholeNumbers")
      )
    )
  }

  val moduleSpec = Specification(specTypes, specValues)

}
