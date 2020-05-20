package morphir.ir.sdk

import morphir.ir.Module.ModulePath
import morphir.ir.Type.Reference
import morphir.ir.{ ModuleSpecification, Name, Value }
import morphir.ir.Type.Specification.OpaqueTypeSpecification

object Int {
  val moduleName: ModulePath =
    ModulePath.fromString("Int")

  val moduleSpec: ModuleSpecification[Unit] = ModuleSpecification(
    Map(
      Name.fromString("Int") -> OpaqueTypeSpecification(List.empty)
    ),
    Map.empty
  )

  def intType[A](attributes: A): Reference[A] =
    Reference(attributes, Common.toFQName(moduleName, "Int"))

  @inline def intType: Reference[Unit] =
    intType(())

  def divide[A](attributes: A): Value.Reference[A] =
    Value.Reference(attributes, Common.toFQName(moduleName, "divide"))
}
