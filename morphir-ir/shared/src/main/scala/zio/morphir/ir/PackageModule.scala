package zio.morphir.ir

import zio.morphir.ir.ModuleModule.{Specification => ModuleSpec, Definition => ModuleDef}

object PackageModule {

  val emptySpecification: Specification[Any] = Specification.empty

  final case class Definition[+Annotations](
      modules: Map[ModuleModule.ModuleName, AccessControlled[ModuleDefinition[Annotations]]]
  ) { self =>
    def toSpecification: Specification[Annotations] = {
      val modules = self.modules.collect { case (moduleName, AccessControlled.WithPublicAccess(moduleDefinition)) =>
        moduleName -> moduleDefinition.toSpecification
      }
      Specification(modules)
    }

    def toSpecificationWithPrivate: Specification[Annotations] = {
      val modules = self.modules.collect { case (moduleName, AccessControlled.WithPrivateAccess(moduleDefinition)) =>
        moduleName -> moduleDefinition.toSpecification
      }
      Specification(modules)
    }

    def lookupModuleDefinition(path: Path): Option[ModuleDef[Annotations]] = lookupModuleDefinition(
      ModuleName.fromPath(path)
    )

    def lookupModuleDefinition(moduleName: ModuleName): Option[ModuleDef[Annotations]] =
      modules.get(moduleName).map(_.withPrivateAccess)

    def lookupTypeDefinition(path: Path, name: Name): Option[ModuleDef[Annotations]] =
      lookupTypeDefinition(ModuleName(path, name))

    def lookupTypeDefinition(moduleName: ModuleName): Option[ModuleDef[Annotations]] =
      modules.get(moduleName).map(_.withPrivateAccess)

    def mapDefinitionAttributes[B](func: Annotations => B): Definition[B] = ???

  }

  object Definition {
    def empty[Annotations]: Definition[Annotations] = Definition(Map.empty)
  }

  final case class Specification[+Annotations](modules: Map[ModuleName, ModuleSpec[Annotations]]) {
    self =>

    def lookupModuleSpecification(path: Path): Option[ModuleSpec[Annotations]] =
      lookupModuleSpecification(ModuleName.fromPath(path))

    def lookupModuleSpecification(moduleName: ModuleName): Option[ModuleSpec[Annotations]] =
      modules.get(moduleName)

    def lookupTypeSpecification(path: Path, name: Name): Option[ModuleSpec[Annotations]] =
      lookupTypeSpecification(ModuleName(path, name))

    def lookupTypeSpecification(moduleName: ModuleName): Option[ModuleSpec[Annotations]] =
      modules.get(moduleName)

    def mapSpecificationAttributes[B](func: Annotations => B): Specification[B] = ???

  }

  object Specification {
    val empty: Specification[Any] = Specification(Map.empty)
  }

  final case class PackageName(toPath: Path) { self =>
    def %(modulePath: ModulePath): PackageAndModulePath = PackageAndModulePath(self, modulePath)
    def %(moduleName: ModuleName): FQName =
      FQName(self, ModulePath(moduleName.namespace), moduleName.localName)
  }

  object PackageName {
    def fromString(input: String): PackageName = PackageName(Path.fromString(input))
  }

  final case class PackageAndModulePath(packageName: PackageName, modulePath: ModulePath) { self =>
    def %(name: Name): FQName = FQName(packageName, modulePath, name)
  }
}

trait PackageSpecFor[A] {
  import PackageModule._

  def packageName: PackageName
  def spec: Specification[Any]
  def nativeFunctions: Map[FQName, NativeFunction]
}

object PackageSpecFor {
  def apply[A](implicit packageSpecFor: PackageSpecFor[A]): PackageSpecFor[A] = packageSpecFor
}
