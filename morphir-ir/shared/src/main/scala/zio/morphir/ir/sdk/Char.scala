package zio.morphir.ir.sdk

import zio.morphir.ir.Module
import zio.morphir.ir.ModuleModule.ModuleName
import zio.morphir.ir.Type.Specification.OpaqueTypeSpecification
import zio.morphir.ir.Type.Type
import zio.morphir.ir.types.UType
import zio.morphir.ir.sdk.Basics.{boolType, intType}
import zio.morphir.ir.sdk.Common.{toFQName, vSpec}
import zio.morphir.syntax.NamingSyntax._
import zio.morphir.ir.Type.Type._

object Char {
  val moduleName: ModuleName = ModuleName.fromString("Char")

  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map(
      name("Char") -> OpaqueTypeSpecification() ?? "Type that represents a single character."
    ),
    values = Map(
      vSpec("isUpper", "c" -> charType)(boolType),
      vSpec("isLower", "c" -> charType)(boolType),
      vSpec("isAlpha", "c" -> charType)(boolType),
      vSpec("isAlphaNum", "c" -> charType)(boolType),
      vSpec("isDigit", "c" -> charType)(boolType),
      vSpec("isOctDigit", "c" -> charType)(boolType),
      vSpec("isHexDigit", "c" -> charType)(boolType),
      vSpec("toUpper", "c" -> charType)(charType),
      vSpec("toLower", "c" -> charType)(charType),
      vSpec("toLocaleUpper", "c" -> charType)(charType),
      vSpec("toLocaleLower", "c" -> charType)(charType),
      vSpec("toCode", "c" -> charType)(intType),
      vSpec("fromCode", ("c" -> intType))(charType)
    )
  )

  lazy val charType: UType = reference(toFQName(moduleName, "Char"))
  def charType[A](attributes: A): Type[A] =
    reference(attributes)(toFQName(moduleName, "Char"))
}
