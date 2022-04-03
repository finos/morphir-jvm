package zio.morphir.ir.sdk

import zio.Chunk
import zio.morphir.ir.Module
import zio.morphir.ir.Module.ModuleName
import zio.morphir.ir.Type.Type
import zio.morphir.ir.types.UType
import zio.morphir.ir.Type.Type._
import zio.morphir.ir.sdk.Basics.boolType
import zio.morphir.ir.sdk.Common._
import zio.morphir.ir.sdk.List.listType
import zio.morphir.ir.sdk.Maybe.maybeType
import zio.morphir.ir.types.Specification.TypeAliasSpecification
import zio.morphir.syntax.NamingSyntax._

object Rule {
  val moduleName: ModuleName = ModuleName.fromString("Rule")

  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map(
      name("Rule") -> TypeAliasSpecification(
        Chunk(name("a"), name("b")),
        tFun(tVar("a"))(maybeType(tVar("b")))
      ) ?? "Type that represents an rule."
    ),
    values = Map(
      vSpec("chain", "rules" -> listType(ruleType(tVar("a"), tVar("b"))))(ruleType(tVar("a"), tVar("b"))),
      vSpec("any", "value" -> tVar("a"))(boolType),
      vSpec("is", "ref" -> tVar("a"), "value" -> tVar("a"))(boolType),
      vSpec("anyOf", "ref" -> listType(tVar("a")), "value" -> tVar("a"))(boolType),
      vSpec("noneOf", "ref" -> listType(tVar("a")), "value" -> tVar("a"))(boolType)
    )
  )

  def ruleType(itemType1: UType, itemType2: UType): UType =
    reference(toFQName(moduleName, "Rule"), itemType1, itemType2)

  def ruleType[A](attributes: A)(itemType1: Type[A], itemType2: Type[A]): Type[A] =
    reference(attributes)(toFQName(moduleName, "Rule"), itemType1, itemType2)
}
