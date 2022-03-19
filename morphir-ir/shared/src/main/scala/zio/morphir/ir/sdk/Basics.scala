package zio.morphir.ir.sdk

import zio.morphir.ir.{Module, Name, UType}
import zio.morphir.ir.Module.ModuleName
import UType.reference
import Common._
import zio.morphir.ir.TypeModule.Specification.OpaqueTypeSpecification

object Basics {
  val moduleName: ModuleName = ModuleName.fromString("Basics")
  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map(
      Name.fromString("Int")   -> OpaqueTypeSpecification() ?? "Type that represents an integer value.",
      Name.fromString("Float") -> OpaqueTypeSpecification() ?? "Type that represents a floating-point number.",
      Name.fromString("Bool")  -> OpaqueTypeSpecification() ?? "Type that represents a boolean value.",
      Name.fromString("Never") -> OpaqueTypeSpecification() ?? "A value that can never happen."
    ),
    values = Map(
      // number
      vSpec("add", "a" -> tVar("number"), "b" -> tVar("number"))(tVar("number")),
      vSpec("subtract", "a" -> tVar("number"), "b" -> tVar("number"))(tVar("number")),
      vSpec("multiply", "a" -> tVar("number"), "b" -> tVar("number"))(tVar("number")),
      vSpec("divide", "a" -> floatType, "b" -> floatType)(floatType),
      vSpec("integerDivide", "a" -> intType, "b" -> intType)(intType),
      vSpec("toFloat", "a" -> intType)(floatType),

      // eq
      vSpec("equal", "a" -> tVar("eq"), "b" -> tVar("eq"))(boolType),
      vSpec("notEqual", "a" -> tVar("eq"), "b" -> tVar("eq"))(boolType),

      // comparable
      vSpec("compare", "a" -> tVar("comparable"), "b" -> tVar("comparable"))(orderType),

      // Bool
      vSpec("not", "a" -> boolType)(boolType)
    )
  )

  lazy val boolType: UType  = reference((toFQName(moduleName, "Bool")))
  lazy val floatType: UType = reference((toFQName(moduleName, "Float")))
  lazy val intType: UType   = reference((toFQName(moduleName, "Int")))
  lazy val orderType: UType = reference((toFQName(moduleName, "Order")))
}
