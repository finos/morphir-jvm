package zio.morphir.ir.sdk

import zio.morphir.ir.Module
import zio.morphir.ir.ModuleModule.ModuleName
import zio.morphir.ir.TypeModule.Specification.OpaqueTypeSpecification
import zio.morphir.ir.TypeModule.{Type, UType}
import zio.morphir.ir.TypeModule.Type._
import zio.morphir.ir.sdk.Basics.{boolType, floatType}
import zio.morphir.ir.sdk.Common._
import zio.morphir.ir.sdk.List.listType
import zio.morphir.syntax.NamingSyntax._

object Aggregate {

  val moduleName: ModuleName = ModuleName.fromString("Aggregate")

  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map(name("Aggregation") -> OpaqueTypeSpecification("a", "key") ?? ""),
    values = Map(
      vSpec("count")(aggregationType(tVar("a"), unit)),
      vSpec("sumOf", "getValue" -> tFun(tVar("a"))(floatType))(aggregationType(tVar("a"), unit)),
      vSpec("averageOf", "getValue" -> tFun(tVar("a"))(floatType))(aggregationType(tVar("a"), unit)),
      vSpec("minimumOf", "getValue" -> tFun(tVar("a"))(floatType))(aggregationType(tVar("a"), unit)),
      vSpec("maximumOf", "getValue" -> tFun(tVar("a"))(floatType))(aggregationType(tVar("a"), unit)),
      vSpec("weightedAverageOf", "getWeight" -> tFun(tVar("a"))(floatType), "getValue" -> tFun(tVar("a"))(floatType))(
        aggregationType(tVar("a"), unit)
      ),
      vSpec("byKey", "key" -> tFun(tVar("a"))(tVar("key")), "agg" -> aggregationType(tVar("a"), unit))(
        aggregationType(tVar("a"), tVar("key"))
      ),
      vSpec("withFilter", "filter" -> tFun(tVar("a"))(boolType), ("agg" -> aggregationType(tVar("a"), tVar("key"))))(
        aggregationType(tVar("a"), tVar("key"))
      ),
      vSpec(
        "aggregateMap",
        "agg1" -> aggregationType(tVar("a"), tVar("key1")),
        "f"    -> tFun(floatType, tVar("a"))(tVar("b")),
        "list" -> listType(tVar("a"))
      )(listType(tVar("b"))),
      vSpec(
        "aggregateMap2",
        "agg1" -> aggregationType(tVar("a"), tVar("key1")),
        "agg2" -> aggregationType(tVar("a"), tVar("key2")),
        "f"    -> tFun(floatType, floatType, tVar("a"))(tVar("b")),
        "list" -> listType(tVar("a"))
      )(listType(tVar("b"))),
      vSpec(
        "aggregateMap3",
        "agg1" -> aggregationType(tVar("a"), tVar("key1")),
        "agg2" -> aggregationType(tVar("a"), tVar("key2")),
        "agg3" -> aggregationType(tVar("a"), tVar("key3")),
        "f"    -> tFun(floatType, floatType, floatType, tVar("a"))(tVar("b")),
        "list" -> listType(tVar("a"))
      )(listType(tVar("b")))
    )
  )

  def aggregationType(aType: UType, keyType: UType): UType =
    reference(toFQName(moduleName, "Aggregation"), aType, keyType)

  def aggregationType[A](attributes: A)(aType: Type[A], keyType: Type[A]): Type[A] =
    reference(attributes)(toFQName(moduleName, "Aggregation"), aType, keyType)
}
