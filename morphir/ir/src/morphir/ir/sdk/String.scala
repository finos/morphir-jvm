package morphir.ir.sdk

import morphir.ir.module.ModulePath
import morphir.ir.{ ModuleSpecification, Name }
import morphir.ir.Type.Reference
import morphir.ir.Type.Specification.OpaqueTypeSpecification

object String {
  val moduleName: ModulePath =
    ModulePath.fromString("String")

  val moduleSpec: ModuleSpecification[Unit] = ModuleSpecification(
    Map(
      Name.fromString("String") -> OpaqueTypeSpecification(List.empty)
    ),
    Map.empty
  )

  def stringType[A](attributes: A): Reference[A] =
    Reference(attributes, Common.toFQName(moduleName, "String"), List.empty)

  @inline def stringType: Reference[Unit] =
    stringType(())
}
