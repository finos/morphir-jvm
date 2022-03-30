package zio.morphir.ir.sdk

import zio.morphir.ir.Module
import zio.morphir.ir.ModuleModule.ModuleName
import zio.morphir.ir.Type.Specification.OpaqueTypeSpecification
import zio.morphir.ir.Type.Type._
import zio.morphir.ir.Type.Type
import zio.morphir.ir.types.UType
import zio.morphir.ir.sdk.Basics.intType
import zio.morphir.ir.sdk.Common._
import zio.morphir.ir.sdk.Maybe.maybeType
import zio.morphir.ir.sdk.String.stringType
import zio.morphir.syntax.NamingSyntax._

object LocalTime {
  val moduleName: ModuleName = ModuleName.fromString("LocalTime")

  val moduleSpec: Module.USpecification = Module.USpecification(
    types = Map(name("LocalTime") -> OpaqueTypeSpecification() ?? "Type that represents a time concept."),
    values = Map(
      vSpec("fromISO", "iso" -> stringType)(maybeType(localTimeType)),
      vSpec("fromMilliseconds", "millis" -> intType)(localTimeType),
      vSpec("diffInSeconds", "timeA" -> localTimeType, "timeB" -> localTimeType)(intType),
      vSpec("diffInMinutes", "timeA" -> localTimeType, "timeB" -> localTimeType)(intType),
      vSpec("diffInHours", "timeA" -> localTimeType, "timeB" -> localTimeType)(intType),
      vSpec("addSeconds", "seconds" -> intType, "time" -> localTimeType)(localTimeType),
      vSpec("addMinutes", "minutes" -> intType, "time" -> localTimeType)(localTimeType),
      vSpec("addHours", "hours" -> intType, "time" -> localTimeType)(localTimeType)
    )
  )

  lazy val localTimeType: UType = reference(toFQName(moduleName, "localTime"))
  def localTimeType[A](attributes: A): Type[A] =
    reference(attributes)(toFQName(moduleName, "localTime"))
}
