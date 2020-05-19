package morphir.ir

import morphir.ir.MorphirPackage.{ PackagePath, Specification }

case class MorphirPackage[+A](
  dependencies: Map[PackagePath, Specification[A]],
  modules: Map[ModulePath, AccessControlled[Module.Definition[A]]]
) {
  def toPackageDefinition: PackageDefinition[A] = Package.Definition(dependencies, modules)
}
object MorphirPackage {
  final case class PackagePath(value: Path) extends AnyVal

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
