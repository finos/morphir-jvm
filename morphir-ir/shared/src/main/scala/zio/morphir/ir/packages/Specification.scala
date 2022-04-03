package zio.morphir.ir.packages
import zio.morphir.ir.{Name, Path}
import zio.morphir.ir.module.{ModuleName, Specification => ModuleSpec}

final case class Specification[+TA](modules: Map[ModuleName, ModuleSpec[TA]]) {
  self =>

  def lookupModuleSpecification(path: Path): Option[ModuleSpec[TA]] =
    lookupModuleSpecification(ModuleName.fromPath(path))

  def lookupModuleSpecification(moduleName: ModuleName): Option[ModuleSpec[TA]] =
    modules.get(moduleName)

  def lookupTypeSpecification(path: Path, name: Name): Option[ModuleSpec[TA]] =
    lookupTypeSpecification(ModuleName(path, name))

  def lookupTypeSpecification(moduleName: ModuleName): Option[ModuleSpec[TA]] =
    modules.get(moduleName)

  def mapSpecificationAttributes[B](func: TA => B): Specification[B] = ???

}

object Specification {
  val empty: Specification[Nothing] = Specification(Map.empty)

  type Raw = Specification[Unit]
  object Raw {
    def apply(modules: Map[ModuleName, ModuleSpec[Unit]]): Raw =
      Specification(modules)
  }
}
