package zio.morphir.ir.sdk

import zio.morphir.ir.Module.ModuleName
import zio.morphir.ir.TypeModule.Specification.{CustomTypeSpecification, OpaqueTypeSpecification}
import zio.morphir.ir.TypeModule.Type
import zio.morphir.ir.UType.{reference, tuple}
import zio.morphir.ir.sdk.Common._
import zio.morphir.ir.{Module, UType}
import zio.morphir.syntax.NamingSyntax._

object Basics {
  val moduleName: ModuleName = ModuleName.fromString("Basics")
  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map(
      name("Int")   -> OpaqueTypeSpecification() ?? "Type that represents an integer value.",
      name("Float") -> OpaqueTypeSpecification() ?? "Type that represents a floating-point number.",
      name("Order") -> CustomTypeSpecification.mkEnum(
        "LT",
        "EQ",
        "GT"
      ) ?? "Represents the relative ordering of two things. The relations are less than, equal to, and greater than.",
      name("Bool")  -> OpaqueTypeSpecification() ?? "Type that represents a boolean value.",
      name("Never") -> OpaqueTypeSpecification() ?? "A value that can never happen."
    ),
    values = Map(
      // number
      vSpec("add", "a" -> tVar("number"), "b" -> tVar("number"))(tVar("number")),
      vSpec("subtract", "a" -> tVar("number"), "b" -> tVar("number"))(tVar("number")),
      vSpec("multiply", "a" -> tVar("number"), "b" -> tVar("number"))(tVar("number")),
      vSpec("divide", "a" -> floatType, "b" -> floatType)(floatType),
      vSpec("integerDivide", "a" -> intType, "b" -> intType)(intType),
      vSpec("power", "a" -> tVar("number"), "b" -> tVar("number"))(tVar("number")),
      vSpec("toFloat", "a" -> intType)(floatType),
      vSpec("round", "a" -> floatType)(intType),
      vSpec("floor", "a" -> floatType)(intType),
      vSpec("ceiling", "a" -> floatType)(intType),
      vSpec("truncat", "a" -> floatType)(intType),
      vSpec("modBy", "a" -> intType, "b" -> intType)(intType),
      vSpec("remainderBy", "a" -> intType, "b" -> intType)(intType),
      vSpec("negate", "a" -> tVar("number"))(tVar("number")),
      vSpec("abs", "a" -> tVar("number"))(tVar("number")),
      vSpec("clamp", "a" -> tVar("number"), "b" -> tVar("number"), "c" -> tVar("number"))(tVar("number")),
      vSpec("isNan", "a" -> floatType)(boolType),
      vSpec("isInfinite", "a" -> floatType)(boolType),
      vSpec("sqrt", "a" -> floatType)(floatType),
      vSpec("logBase", "a" -> floatType, "b" -> floatType)(floatType),
      vSpec("e")(floatType),
      vSpec("pi")(floatType),
      vSpec("cos", "a" -> floatType)(floatType),
      vSpec("sin", "a" -> floatType)(floatType),
      vSpec("tan", "a" -> floatType)(floatType),
      vSpec("acos", "a" -> floatType)(floatType),
      vSpec("asin", "a" -> floatType)(floatType),
      vSpec("atan", "a" -> floatType)(floatType),
      vSpec("atan2", "a" -> floatType, "b" -> floatType)(floatType),
      vSpec("degrees", "a" -> floatType)(floatType),
      vSpec("radians", "a" -> floatType)(floatType),
      vSpec("turns", "a" -> floatType)(floatType),
      vSpec("toPolar", "a" -> tuple(floatType, floatType))(tuple(floatType, floatType)),
      vSpec("fromPolar", "a" -> tuple(floatType, floatType))(tuple(floatType, floatType)),
      // eq
      vSpec("equal", "a" -> tVar("eq"), "b" -> tVar("eq"))(boolType),
      vSpec("notEqual", "a" -> tVar("eq"), "b" -> tVar("eq"))(boolType),

      // comparable
      vSpec("lessThan", "a" -> tVar("comparable"), "b" -> tVar("comparable"))(boolType),
      vSpec("greaterThan", "a" -> tVar("comparable"), "b" -> tVar("comparable"))(boolType),
      vSpec("lessThanOrEqual", "a" -> tVar("comparable"), "b" -> tVar("comparable"))(boolType),
      vSpec("greaterThanOrEqual", "a" -> tVar("comparable"), "b" -> tVar("comparable"))(boolType),
      vSpec("max", "a" -> tVar("comparable"), "b" -> tVar("comparable"))(tVar("comparable")),
      vSpec("min", "a" -> tVar("comparable"), "b" -> tVar("comparable"))(tVar("comparable")),
      vSpec("compare", "a" -> tVar("comparable"), "b" -> tVar("comparable"))(orderType),

      // Bool
      vSpec("not", "a" -> boolType)(boolType),
      vSpec("and", "a" -> boolType, "b" -> boolType)(boolType),
      vSpec("or", "a" -> boolType, "b" -> boolType)(boolType),
      vSpec("xor", "a" -> boolType, "b" -> boolType)(boolType),

      // Appendable
      vSpec("append", "a" -> tVar("appendable"), "b" -> tVar("appendable"))(tVar("appendable")),

      // Break
      vSpec("identity", "a" -> tVar("a"))(tVar("a")),
      vSpec("always", "a" -> tVar("a"), "b" -> tVar("b"))(tVar("a")),
      vSpec("composeLeft", "g" -> tFun(tVar("b"))(tVar("c")), "f" -> tFun(tVar("a"))(tVar("b"))).returning(
        tFun(tVar("a"))(tVar("c"))
      ),
      vSpec("composeRight", "f" -> tFun(tVar("a"))(tVar("b")), "g" -> tFun(tVar("b"))(tVar("c"))).returning(
        tFun(tVar("a"))(tVar("c"))
      ),
      vSpec("never", "a" -> neverType)(tVar("a"))
    )
  )

  lazy val boolType: UType                 = reference((toFQName(moduleName, "Bool")))
  def boolType[A](attributes: A): Type[A]  = reference(attributes)((toFQName(moduleName, "Bool")))
  lazy val floatType: UType                = reference((toFQName(moduleName, "Float")))
  lazy val intType: UType                  = reference((toFQName(moduleName, "Int")))
  lazy val neverType: UType                = reference((toFQName(moduleName, "Never")))
  lazy val orderType: UType                = orderType(UType.emptyAttributes)
  def orderType[A](attributes: A): Type[A] = reference(attributes)((toFQName(moduleName, "Order")))
}
