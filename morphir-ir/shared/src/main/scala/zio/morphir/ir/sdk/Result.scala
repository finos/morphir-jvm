package zio.morphir.ir.sdk

import zio.Chunk
import zio.morphir.ir.Module
import zio.morphir.ir.ModuleModule.ModuleName
import zio.morphir.ir.TypeModule.{Constructors, Type, UType}
import zio.morphir.ir.types.Type._
import zio.morphir.ir.ValueModule.Value
import zio.morphir.ir.ValueModule.Value.constructor
import zio.morphir.ir.ValueModule.ValueCase.ApplyCase
import zio.morphir.ir.sdk.Common._
import zio.morphir.ir.sdk.Maybe.maybeType
import zio.morphir.ir.types.Specification.CustomTypeSpecification
import zio.morphir.syntax.NamingSyntax._

object Result {
  val moduleName: ModuleName = ModuleName.fromString("Result")

  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map(
      name("Result") ->
        CustomTypeSpecification(
          Chunk(name("e"), name("a")),
          Constructors(
            Map(
              name("Ok")  -> Chunk((name("value"), variable(name("a")))),
              name("Err") -> Chunk((name("error"), variable(name("e"))))
            )
          )
        ) ?? "Type that represents the result of a computation that can either succeed or fail."
    ),
    values = Map(
      vSpec(
        "andThen",
        "f"      -> tFun(tVar("a"))(resultType(tVar("x"), tVar("b"))),
        "result" -> resultType(tVar("x"), tVar("a"))
      )(resultType(tVar("x"), tVar("b"))),
      vSpec("map", "f" -> tFun(tVar("a"))(tVar("b")), "result" -> resultType(tVar("x"), tVar("a")))(
        resultType(tVar("x"), tVar("b"))
      ),
      vSpec(
        "map2",
        "f"       -> tFun(tVar("a"), tVar("b"))(tVar("r")),
        "result1" -> resultType(tVar("x"), tVar("a")),
        "result2" -> resultType(tVar("x"), tVar("b"))
      )(resultType(tVar("x"), tVar("r"))),
      vSpec(
        "map3",
        "f"       -> tFun(tVar("a"), tVar("b"), tVar("c"))(tVar("r")),
        "result1" -> resultType(tVar("x"), tVar("a")),
        "result2" -> resultType(tVar("x"), tVar("b")),
        "result2" -> resultType(tVar("x"), tVar("c"))
      )(resultType(tVar("x"), tVar("r"))),
      vSpec(
        "map4",
        "f"       -> tFun(tVar("a"), tVar("b"), tVar("c"), tVar("d"))(tVar("r")),
        "result1" -> resultType(tVar("x"), tVar("a")),
        "result2" -> resultType(tVar("x"), tVar("b")),
        "result2" -> resultType(tVar("x"), tVar("c")),
        "result2" -> resultType(tVar("x"), tVar("d"))
      )(resultType(tVar("x"), tVar("r"))),
      vSpec(
        "map5",
        "f"       -> tFun(tVar("a"), tVar("b"), tVar("c"), tVar("d"), tVar("e"))(tVar("r")),
        "result1" -> resultType(tVar("x"), tVar("a")),
        "result2" -> resultType(tVar("x"), tVar("b")),
        "result2" -> resultType(tVar("x"), tVar("c")),
        "result2" -> resultType(tVar("x"), tVar("d")),
        "result2" -> resultType(tVar("x"), tVar("e"))
      )(resultType(tVar("x"), tVar("r"))),
      vSpec("withDefault", "default" -> tVar("a"), "result" -> resultType(tVar("x"), tVar("a")))(tVar("a")),
      vSpec("toMaybe", "result" -> resultType(tVar("x"), tVar("a")))(maybeType(tVar("a"))),
      vSpec("fromMaybe", "error" -> tVar("x"), "maybe" -> maybeType(tVar("a")))(resultType(tVar("x"), tVar("a"))),
      vSpec("mapError", "f" -> tFun(tVar("x"))(tVar("y")), "result" -> resultType(tVar("x"), tVar("a")))(
        resultType(tVar("y"), tVar("a"))
      )
    )
  )

  def resultType(errorType: UType, itemType: UType): UType =
    reference(toFQName(moduleName, "result"), errorType, itemType)

  def resultType[A](attributes: A)(errorType: Type[A], itemType: Type[A]): Type[A] =
    reference(attributes)(toFQName(moduleName, "result"), errorType, itemType)

  def ok(value: Value[Any]): Value[Any] =
    Value(ApplyCase(constructor(toFQName(moduleName, "Ok")), Chunk(value)))

  def ok[A](va: A)(value: Value[A]): Value[A] =
    Value(ApplyCase(constructor(toFQName(moduleName, "Ok"), va), Chunk(value)), va)

  def err(error: Value[Any]): Value[Any] =
    Value(ApplyCase(constructor(toFQName(moduleName, "Err")), Chunk(error)))

  def err[A](va: A)(error: Value[A]): Value[A] =
    Value(ApplyCase(constructor(toFQName(moduleName, "Err"), va), Chunk(error)), va)

  // todo add nativefunctions
}
