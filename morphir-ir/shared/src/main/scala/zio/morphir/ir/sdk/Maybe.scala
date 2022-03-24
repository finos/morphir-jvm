package zio.morphir.ir.sdk

import zio.Chunk
import zio.morphir.ir.Module
import zio.morphir.ir.ModuleModule.ModuleName
import zio.morphir.ir.TypeModule.Specification.CustomTypeSpecification
import zio.morphir.ir.TypeModule.{Type, UConstructors, UType}
import zio.morphir.ir.TypeModule.Type._
import zio.morphir.ir.ValueModule.Value
import zio.morphir.ir.ValueModule.Value.constructor
import zio.morphir.ir.ValueModule.ValueCase.ApplyCase
import zio.morphir.ir.sdk.Common._
import zio.morphir.syntax.NamingSyntax._

object Maybe {
  val moduleName: ModuleName = ModuleName.fromString("Maybe")

  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map(
      name("Maybe") -> CustomTypeSpecification(
        Chunk(name("a")),
        UConstructors(
          Map(name("Just") -> Chunk((name("value"), variable(name("a")))), name("Nothing") -> Chunk.empty)
        )
      ) ?? "Type that represents an optional value."
    ),
    values = Map(
      vSpec("andThen", "f" -> tFun(tVar("a"))(maybeType(tVar("b"))), "maybe" -> maybeType(tVar("a")))(
        maybeType(tVar("b"))
      ),
      vSpec("map", "f" -> tFun(tVar("a"))(tVar("b")), "maybe" -> maybeType(tVar("a")))(maybeType(tVar("b"))),
      vSpec(
        "map2",
        "f"      -> tFun(tVar("a"), tVar("b"))(tVar("r")),
        "maybe1" -> maybeType(tVar("a")),
        "maybe2" -> maybeType(tVar("b"))
      )(maybeType(tVar("r"))),
      vSpec(
        "map3",
        "f"      -> tFun(tVar("a"), tVar("b"), tVar("c"))(tVar("r")),
        "maybe1" -> maybeType(tVar("a")),
        "maybe2" -> maybeType(tVar("b")),
        "maybe3" -> maybeType(tVar("c"))
      )(maybeType(tVar("r"))),
      vSpec(
        "map4",
        "f"      -> tFun(tVar("a"), tVar("b"), tVar("c"), tVar("d"))(tVar("r")),
        "maybe1" -> maybeType(tVar("a")),
        "maybe2" -> maybeType(tVar("b")),
        "maybe3" -> maybeType(tVar("c")),
        "maybe4" -> maybeType(tVar("d"))
      )(maybeType(tVar("r"))),
      vSpec(
        "map5",
        "f"      -> tFun(tVar("a"), tVar("b"), tVar("c"), tVar("d"), tVar("e"))(tVar("r")),
        "maybe1" -> maybeType(tVar("a")),
        "maybe2" -> maybeType(tVar("b")),
        "maybe3" -> maybeType(tVar("c")),
        "maybe4" -> maybeType(tVar("d")),
        "maybe5" -> maybeType(tVar("e"))
      )(maybeType(tVar("r"))),
      vSpec("withDefault", "default" -> tVar("a"), "maybe" -> maybeType(tVar("a")))(tVar("a"))
    )
  )

  def maybeType(itemType: UType): UType =
    reference(toFQName(moduleName, "Maybe"), itemType)

  def maybeType[A](attributes: A)(itemType: Type[A]): Type[A] =
    reference(attributes)(toFQName(moduleName, "Maybe"), itemType)

  def just(value: Value[Any]): Value[Any] =
    Value(ApplyCase(constructor(toFQName(moduleName, "Just")), Chunk(value)))

  def just[A](va: A)(value: Value[A]): Value[A] =
    Value(ApplyCase(constructor(toFQName(moduleName, "Just"), va), Chunk(value)), va)

  lazy val nothing: Value[Any] =
    constructor(toFQName(moduleName, "Nothing"))
  def nothing[A](va: A): Value[A] =
    constructor(toFQName(moduleName, "Nothing"), va)

  // todo add nativeFunctions
}
