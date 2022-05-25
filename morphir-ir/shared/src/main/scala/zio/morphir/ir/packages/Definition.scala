package zio.morphir.ir.packages

import zio.morphir.ir.Module.{Definition => ModuleDef, ModuleName}
import zio.morphir.ir.Type.UType
import zio.morphir.ir.Value.{Definition => ValueDef}
import zio.morphir.ir.{AccessControlled, Name, Path}

final case class Definition[+TA, +VA](
    modules: Map[ModuleName, AccessControlled[ModuleDef[TA, VA]]]
) { self =>

  def eraseAttributes: Definition[Any, Any] = self.mapAttributes(_ => (), _ => ())

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

  def mapAttributes[TB, VB](tf: TA => TB, vf: VA => VB): Definition[TB, VB] = Definition(
    modules.map { case (name, moduleDef) => (name, moduleDef.map(_.mapAttributes(tf, vf))) }
  )

  def lookupValueDefinition(path: Path, name: Name): Option[ValueDef[TA, VA]] =
    self.lookupModuleDefinition(path).flatMap(_.lookupValueDefinition(name))

//  @tailrec
//  def selectModules(
//      modulesToInclude: Set[ModuleName],
//      name: PackageName
//  ): Definition[TA, VA] = {
//    val expandedMods = expandedModulesToInclude(modulesToInclude, name)
//
//    if (modulesToInclude == expandedMods) {
//      Definition(modules.filter(module => modulesToInclude.contains(module._1)))
//    } else selectModules(expandedMods, name)
//  }
//
//  private def expandedModulesToInclude(modulesToInclude: Set[ModuleName], name: PackageName): Set[ModuleName] =
//    findAllDependencies(modulesToInclude, name) ++ modulesToInclude
//
//  private def findAllDependencies(
//      current: Set[ModuleName],
//      name: PackageName
//  ): Set[ModuleName] =
//    current
//      .foldLeft(Set.empty[ModuleName])((set, moduleName) =>
//        modules
//          .get(moduleName)
//          .foldLeft(Set.empty[ModuleName]) { case (modDepSet, AccessControlled(_, modDef)) =>
//            modDepSet ++ getModuleDependencies(modDef, name)
//          } ++ set
//      )
//
//  private def getModuleDependencies[TA, VA](modDef: ModuleDef[TA, VA], name: PackageName): Set[ModuleName] =
//    modDef.dependsOnModules.flatMap { case QualifiedModuleName(packageName, module) =>
//      if (packageName == name.toPath) Some(ModuleName.fromPath(module))
//      else None
//    }

}

object Definition {
  val empty: Definition[Nothing, Nothing] = Definition(Map.empty)

  type Typed = Definition[Unit, UType]
  object Typed {
    def apply(modules: Map[ModuleName, AccessControlled[ModuleDef[Unit, UType]]]): Definition[Unit, UType] =
      Definition(modules)
  }
}
