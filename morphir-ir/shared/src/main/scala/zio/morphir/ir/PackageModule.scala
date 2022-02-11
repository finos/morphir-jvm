package zio.morphir.ir
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
  }

  object Definition {
    def empty[Annotations]: Definition[Annotations] = Definition(Map.empty)
  }

  final case class Specification[+Annotations](modules: Map[ModuleName, ModuleModule.Specification[Annotations]])
  object Specification {
    val empty: Specification[Any] = Specification(Map.empty)
  }

  final case class PackageName(toPath: Path) { self =>
    def %(modulePath: ModulePath): PackageAndModulePath = PackageAndModulePath(self, modulePath)
    def %(moduleName: ModuleName): FQName =
      FQName(self, ModulePath(moduleName.namespace), moduleName.localName)
  }

  final case class PackageAndModulePath(packageName: PackageName, modulePath: ModulePath) { self =>
    def %(name: Name): FQName = FQName(packageName, modulePath, name)
  }
}

trait PackageSpecFor[A] {
  import PackageModule.*

  def packageName: PackageName
  def spec: Specification[Any]
  def nativeFunctions: Map[FQName, NativeFunction]
}

object PackageSpecFor {
  def apply[A](implicit packageSpecFor: PackageSpecFor[A]): PackageSpecFor[A] = packageSpecFor
}
