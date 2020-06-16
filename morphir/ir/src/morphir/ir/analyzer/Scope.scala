package morphir.ir.analyzer

import morphir.ir.Name
import morphir.ir.MorphirPackage.Pkg

sealed trait ParentScope[+C] {
  def children: Map[Name, Scope[C]]
  def childScopes: Iterable[Scope[C]] = children.values
}

sealed trait Scope[+A] {
  type Parent

  def data: A
  def parent: Parent
  //def map[S1](f: S => S1): Scope.Aux[S1, ParentScope]
}

object Scope {

  def pkg[A](data: A): PackageScope[A] = PackageScope(data)
  def fromPackage[A, D](pkg: Pkg[A])(toData: Pkg[A] => D): PackageScope[D] =
    PackageScope(toData(pkg))
}

final case class PackageScope[+A](data: A) extends Scope[A] {
  type Parent = Unit
  val parent: Parent = ()

}

sealed trait PackageRootScope[+A] extends Scope[A] {
  type Parent <: PackageScope[_]
}

final case class PackageDependencyScope[+A, D](data: A, parent: PackageScope[D]) extends PackageRootScope[A] {
  type Parent = PackageScope[D]
}

sealed trait ModuleScope[+A] extends Scope[A]
object ModuleScope {
  final case class RootModuleScope[+A, P](data: A, parent: PackageScope[P]) extends ModuleScope[A] {
    type Parent = PackageScope[P]
  }

  final case class ChildModuleScope[+A, P](data: A, parent: ModuleScope[P]) extends ModuleScope[A] {
    type Parent = ModuleScope[P]
  }
}
