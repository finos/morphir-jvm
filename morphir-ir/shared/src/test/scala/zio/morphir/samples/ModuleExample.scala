package zio.morphir.samples

import zio.Chunk
import zio.morphir.Dsl.define
import zio.morphir.ir.Module.{Definition, Specification}
import zio.morphir.ir.Type.Definition.{CustomType, TypeAlias}
import zio.morphir.ir.Type.Specification.OpaqueTypeSpecification
import zio.morphir.ir.Type.{Constructors, UType}
import zio.morphir.ir.Value.ValueDefinition
import zio.morphir.ir.{AccessControlled, Documented, Literal => Lit, Name, Value, value}

object ModuleExample {
  val items: Map[Name, Chunk[(Name, UType)]] = Map {
    Name("type")    -> Chunk((Name("var"), define variable ("var1")))
    Name("rainbow") -> Chunk((Name("red"), define variable ("red")))
  }

  val typeAlias: Documented[TypeAlias[Any]] = Documented(
    "doc",
    TypeAlias(Chunk(Name.fromString("hello")), define.variable("type1"))
  )

  val customType: Documented[CustomType[Any]] = Documented(
    "doc",
    CustomType[Any](Chunk(Name.fromString("world")), AccessControlled.publicAccess(Constructors(items)))
  )

  val definitionTypes: Map[Name, AccessControlled[Documented[CustomType[Any]]]] = Map {
    Name("hello") -> AccessControlled.publicAccess(typeAlias)
    Name("world") -> AccessControlled.publicAccess(customType)
  }

  val definitionValues: Map[Name, AccessControlled[Documented[ValueDefinition.Typed]]] = Map {
    Name("val") -> AccessControlled.publicAccess(
      Documented("type", ValueDefinition.fromLiteral(Lit.string("string")))
    )
  }

  val moduleDef: Definition[Any, UType] = Definition(definitionTypes, definitionValues)

  val specTypes: Map[Name, Documented[OpaqueTypeSpecification]] = Map {
    Name("hello") -> Documented(
      "doc",
      OpaqueTypeSpecification(Chunk(Name("name1")))
    )
    Name("world") -> Documented(
      "doc",
      OpaqueTypeSpecification(Chunk(Name("name2")))
    )
  }

  val specValues: Map[Name, Documented[value.Specification[Any]]] = Map {
    Name("spec1") -> Documented(
      "types",
      Value.Specification(
        Chunk(
          (Name("type1"), define.variable("Float")),
          (Name("type2"), define.variable("Decimal"))
        ),
        define.variable("WholeNumbers")
      )
    )
  }

  val moduleSpec: Specification[Any] = Specification(specTypes, specValues)

}
