package zio.morphir.ir.packages

import zio.morphir.ir.Type.UType
import zio.morphir.ir.module.{Definition => ModuleDef, ModuleName}
import zio.morphir.ir.{AccessControlled, Name, Path}

final case class Definition[+TA, +VA](
    modules: Map[ModuleName, AccessControlled[ModuleDef[TA, VA]]]
) { self =>
  def toSpecification: Specification[TA] = {
    val modules = self.modules.collect { case (moduleName, AccessControlled.WithPublicAccess(moduleDefinition)) =>
      moduleName -> moduleDefinition.toSpecification
    }
    Specification(modules)
  }

  def toSpecificationWithPrivate: Specification[TA] = {
    val modules = self.modules.collect { case (moduleName, AccessControlled.WithPrivateAccess(moduleDefinition)) =>
      moduleName -> moduleDefinition.toSpecification
    }
    Specification(modules)
  }

  def lookupModuleDefinition(path: Path): Option[ModuleDef[TA, VA]] = lookupModuleDefinition(
    ModuleName.fromPath(path)
  )

  def lookupModuleDefinition(moduleName: ModuleName): Option[ModuleDef[TA, VA]] =
    modules.get(moduleName).map(_.withPrivateAccess)

  def lookupTypeDefinition(path: Path, name: Name): Option[ModuleDef[TA, VA]] =
    lookupTypeDefinition(ModuleName(path, name))

  def lookupTypeDefinition(moduleName: ModuleName): Option[ModuleDef[TA, VA]] =
    modules.get(moduleName).map(_.withPrivateAccess)

  def mapDefinitionAttributes[TB, VB](tf: TA => VB, vf: VA => VB): Definition[TB, VB] = ???

}

object Definition {
  val empty: Definition[Nothing, Nothing] = Definition(Map.empty)

  type Typed = Definition[Unit, UType]
  object Typed {
    def apply(modules: Map[ModuleName, AccessControlled[ModuleDef[Unit, UType]]]): Definition[Unit, UType] =
      Definition(modules)
  }
}
