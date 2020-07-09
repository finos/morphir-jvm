package morphir.ir.sdk

import morphir.ir.module.ModulePath
import morphir.ir.documented
import morphir.ir.Type.Reference
import morphir.ir.{ ModuleSpecification, Name, Value }
import morphir.ir.Type.Specification.OpaqueTypeSpecification

object LocalDate {
  val moduleName: ModulePath =
    ModulePath.fromString("LocalDate")

  val moduleSpec: ModuleSpecification[Unit] = ModuleSpecification(
    Map(
      Name
        .fromString("LocalDate") -> documented("Type that represents an LocalDate value.", OpaqueTypeSpecification(List.empty))
    ),
    Map.empty
  )

  def localDateType[A](attributes: A): Reference[A] =
    Reference(attributes, Common.toFQName(moduleName, "LocalDate"))

  @inline def localDateType: Reference[Unit] =
    LocalDateType(())
}
