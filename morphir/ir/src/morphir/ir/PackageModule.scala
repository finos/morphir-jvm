package morphir.ir

import io.estatico.newtype.macros.newtype

object PackageModule {

  @newtype case class PackagePath(value: Path)

  sealed trait PackageRef
  object PackageRef {
    case object ThisPackage                                        extends PackageRef
    final case class PackageDependency(toPackagePath: PackagePath) extends PackageRef
  }

  case class Pkg[+A](
    dependencies: Map[PackagePath, Specification[A]],
    modules: Map[ModulePath, AccessControlled[Module.Definition[A]]]
  ) {
    def toPackageDefinition: PkgDef[A] = Package.Definition[A](dependencies, modules)
  }

  object Pkg {}

  final case class Specification[+A](modules: Map[ModulePath, Module.Specification[A]])
  object Specification {
    def empty[A]: Specification[A] = Specification[A](Map.empty)
  }

  final case class Definition[+A](
    dependencies: Map[PackagePath, Specification[A]],
    modules: Map[ModulePath, AccessControlled[Module.Definition[A]]]
  )
  object Definition {
    def empty[A]: Definition[A] = Definition(Map.empty, Map.empty)
  }

  @inline def emptySpecification[A]: Specification[A] = Specification.empty
  @inline def emptyDefinition[A]: Definition[A]       = Definition.empty

}
