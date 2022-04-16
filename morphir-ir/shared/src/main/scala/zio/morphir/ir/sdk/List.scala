package zio.morphir.ir.sdk

import zio.Chunk
import zio.morphir.ir.Module.ModuleName
import zio.morphir.ir.Type.Specification.OpaqueTypeSpecification
import zio.morphir.ir.Type.{Type, UType, reference => typeRef, tuple}
import zio.morphir.ir.Value.{Value, reference => valRef}
import zio.morphir.ir.sdk.Basics.{boolType, intType, orderType}
import zio.morphir.ir.sdk.Common._
import zio.morphir.ir.sdk.Maybe.maybeType
import zio.morphir.ir.{Module, NeedsAttributes}
import zio.morphir.syntax.NamingSyntax._

object List {
  val moduleName: ModuleName = ModuleName.fromString("List")

  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map(name("List") -> OpaqueTypeSpecification("a") ?? "Type that represents a list of values."),
    values = Map(
      vSpec("singleton", "a" -> tVar("a"))(listType(tVar("a"))),
      vSpec("repeat", "n" -> intType, "a" -> tVar("a"))(listType(tVar("a"))),
      vSpec("range", "from" -> intType, "to" -> intType)(listType(intType)),
      vSpec("cons", "head" -> tVar("a"), "tail" -> listType(tVar("a")))(listType(tVar("a"))),
      vSpec("map", "f" -> tFun(tVar("a"))(tVar("b")), "list" -> listType(tVar("a")))(listType(tVar("b"))),
      vSpec("indexedMap", "f" -> tFun(intType, tVar("a"))(tVar("b")), "list" -> listType(tVar("a")))(
        listType(tVar("b"))
      ),
      vSpec("foldl", "f" -> tFun(tVar("a"), tVar("b"))(tVar("b")), "z" -> tVar("b"), "list" -> listType(tVar("a")))(
        tVar("b")
      ),
      vSpec("foldr", "f" -> tFun(tVar("a"), tVar("b"))(tVar("b")), "z" -> tVar("b"), "list" -> listType(tVar("a")))(
        tVar("b")
      ),
      vSpec("filter", "f" -> tFun(tVar("a"))(boolType), "list" -> listType(tVar("a")))(listType(tVar("a"))),
      vSpec("filterMap", "f" -> tFun(tVar("a"))(maybeType(tVar("b"))), "list" -> listType(tVar("a")))(
        listType(tVar("b"))
      ),
      vSpec("length", "list" -> listType(tVar("a")))(intType),
      vSpec("reverse", "list" -> listType(tVar("a")))(listType(tVar("a"))),
      vSpec("member", "ref" -> tVar("a"), "list" -> listType(tVar("a")))(boolType),
      vSpec("all", "f" -> tFun(tVar("a"))(boolType), "list" -> listType(tVar("a")))(boolType),
      vSpec("any", "f" -> tFun(tVar("a"))(boolType), "list" -> listType(tVar("a")))(boolType),
      vSpec("maximum", "list" -> listType(tVar("comparable")))(maybeType(tVar("comparable"))),
      vSpec("minimum", "list" -> listType(tVar("comparable")))(maybeType(tVar("comparable"))),
      vSpec("sum", "list" -> listType(tVar("number")))(tVar("number")),
      vSpec("product", "list" -> listType(tVar("number")))(tVar("number")),
      vSpec("append", "l1" -> listType(tVar("a")), "l2" -> listType(tVar("a")))(listType(tVar("a"))),
      vSpec("concat", "lists" -> listType(listType(tVar("a"))))(listType(tVar("a"))),
      vSpec("concatMap", "f" -> tFun(tVar("a"))(listType(tVar("b"))), "list" -> listType(tVar("a")))(
        listType(tVar("b"))
      ),
      vSpec("intersperse", "a" -> tVar("a"), "list" -> listType(tVar("a")))(listType(tVar("a"))),
      vSpec(
        "map2",
        "f"     -> tFun(tVar("a"), tVar("b"))(tVar("r")),
        "list1" -> listType(tVar("a")),
        "list2" -> listType(tVar("b"))
      )(listType(tVar("r"))),
      vSpec(
        "map3",
        "f"     -> tFun(tVar("a"), tVar("b"), tVar("c"))(tVar("r")),
        "list1" -> listType(tVar("a")),
        "list2" -> listType(tVar("b")),
        "list3" -> listType(tVar("c"))
      )(listType(tVar("r"))),
      vSpec(
        "map4",
        "f"     -> tFun(tVar("a"), tVar("b"), tVar("c"), tVar("d"))(tVar("r")),
        "list1" -> listType(tVar("a")),
        "list2" -> listType(tVar("b")),
        "list3" -> listType(tVar("c")),
        "list4" -> listType(tVar("d"))
      )(listType(tVar("r"))),
      vSpec(
        "map5",
        "f" ->
          tFun(tVar("a"), tVar("b"), tVar("c"), tVar("d"), tVar("e"))(tVar("r")),
        "list1" -> listType(tVar("a")),
        "list2" -> listType(tVar("b")),
        "list3" -> listType(tVar("c")),
        "list4" -> listType(tVar("d")),
        "list5" -> listType(tVar("e"))
      )(listType(tVar("r"))),
      vSpec("sort", "list" -> listType(tVar("comparable")))(listType(tVar("comparable"))),
      vSpec("sortBy", "f" -> tFun(tVar("a"))(tVar("comparable")), "list" -> listType(tVar("a")))(listType(tVar("a"))),
      vSpec("sortWith", "f" -> tFun(tVar("a"), tVar("a"))(orderType), "list" -> listType(tVar("a")))(
        listType(tVar("a"))
      ),
      vSpec("isEmpty", "list" -> listType(tVar("a")))(boolType),
      vSpec("head", "list" -> listType(tVar("a")))(maybeType(tVar("a"))),
      vSpec("tail", "list" -> listType(tVar("a")))(maybeType(listType(tVar("a")))),
      vSpec("take", "n" -> intType, "list" -> listType(tVar("a")))(listType(tVar("a"))),
      vSpec("drop", "n" -> intType, "list" -> listType(tVar("a")))(listType(tVar("a"))),
      vSpec("partition", "f" -> tFun(tVar("a"))(boolType), "list" -> listType(tVar("a")))(
        tuple(Chunk(listType(tVar("a")), listType(tVar("a"))))
      ),
      vSpec("unzip", "list" -> listType(tuple(Chunk(tVar("a"), tVar("b")))))(
        tuple(Chunk(listType(tVar("a")), listType(tVar("b"))))
      )
    )
  )

  def listType(itemType: UType): UType =
    typeRef(toFQName(moduleName, "List"), Chunk(itemType))

  def listType[A](attributes: A)(itemType: Type[A])(implicit ev: NeedsAttributes[A]): Type[A] =
    typeRef(attributes, toFQName(moduleName, "List"), itemType)

  def construct[VA](attributes: VA): Value[Nothing, VA] =
    valRef(attributes = attributes, name = toFQName(moduleName, "cons"))
}
