package zio.morphir.ir.sdk

import zio.Chunk
import zio.morphir.ir.Module
import zio.morphir.ir.ModuleModule.ModuleName
import zio.morphir.ir.Type.Specification.OpaqueTypeSpecification
import zio.morphir.ir.Type.Type
import zio.morphir.ir.types.UType
import zio.morphir.ir.Type.Type._
import zio.morphir.ir.sdk.Basics.{boolType, intType}
import zio.morphir.ir.sdk.Common._
import zio.morphir.ir.sdk.List.listType
import zio.morphir.syntax.NamingSyntax._

object Set {
  val moduleName: ModuleName = ModuleName.fromString("Set")

  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map(name("Set") -> OpaqueTypeSpecification("a") ?? "Type that represents a set."),
    values = Map(
      vSpec("empty")(setType(tVar("a"))),
      vSpec("singleton", "a" -> tVar("comparable"))(setType(tVar("comparable"))),
      vSpec("insert", "a" -> tVar("comparable"), "set" -> setType(tVar("comparable")))(setType(tVar("comparable"))),
      vSpec("remove", "a" -> tVar("comparable"), "set" -> setType(tVar("comparable")))(setType(tVar("comparable"))),
      vSpec("isEmpty", "set" -> setType(tVar("comparable")))(boolType),
      vSpec("member", "a" -> tVar("comparable"), "set" -> setType(tVar("comparable")))(boolType),
      vSpec("size", "set" -> setType(tVar("comparable")))(intType),
      vSpec("toList", "set" -> setType(tVar("a")))(listType(tVar("a"))),
      vSpec("fromList", "list" -> listType(tVar("comparable")))(setType(tVar("comparable"))),
      vSpec("map", "f" -> tFun(tVar("comparable"))(tVar("comparable2")), "set" -> setType(tVar("comparable")))(
        setType(tVar("comparable2"))
      ),
      vSpec("foldl", "f" -> tFun(tVar("a"), tVar("b"))(tVar("b")), "z" -> tVar("b"), "set" -> setType(tVar("a")))(
        tVar("b")
      ),
      vSpec("foldr", "f" -> tFun(tVar("a"), tVar("b"))(tVar("b")), "z" -> tVar("b"), "set" -> setType(tVar("a")))(
        tVar("b")
      ),
      vSpec("filter", "f" -> tFun(tVar("comparable"))(boolType), "set" -> setType(tVar("comparable")))(
        setType(tVar("comparable"))
      ),
      vSpec("partition", "f" -> tFun(tVar("comparable"))(boolType), "set" -> setType(tVar("comparable")))(
        tuple(Chunk(setType(tVar("comparable")), setType(tVar("comparable"))))
      ),
      vSpec("union", "set1" -> setType(tVar("comparable")), "set2" -> setType(tVar("comparable")))(
        setType(tVar("comparable"))
      ),
      vSpec("intersect", "set1" -> setType(tVar("comparable")), "set2" -> setType(tVar("comparable")))(
        setType(tVar("comparable"))
      ),
      vSpec("diff", "set1" -> setType(tVar("comparable")), "set2" -> setType(tVar("comparable")))(
        setType(tVar("comparable"))
      )
    )
  )

  def setType(itemType: UType): UType =
    reference(toFQName(moduleName, "set"), itemType)

  def setType[A](attributes: A)(itemType: Type[A]): Type[A] =
    reference(attributes)(toFQName(moduleName, "set"), itemType)
}
