package morphir.ir.analyzer

import morphir.ir.Name
import morphir.ir.PackageModule.Pkg

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

//  type Aux[+S, ParentScope0] = Scope[S] {
//    type ParentScope = ParentScope0
//  }

//  case class Global[+S](state: S) extends Scope[S] {
//    type ParentScope = Unit
//
//    def state: Unit         = ()
//    def parent: ParentScope = ()
//    //def map[S1](f: Unit => S1): Scope.Aux[S1, ParentScope] = ???
//
//  }

  // final case class PackageScope[S](state: S) extends Scope[S] {
  //   type ParentScope = Nothing
  //   def map[S1](f: S => S1): Scope.Aux[S1, ParentScope] = PackageScope(f(state))
  // }
  // final case class ModuleScope[S](modulePath: ModulePath, state: S) extends Scope[S] {
  //   type ParentScope = PackageScope[S]
  //   def map[S1](f: S => S1): Scope[S1] = ModuleScope(modulePath, f(state))
  //   def mapScope[S1](f: (ModulePath, S) => (ModulePath, S1)): ModuleScope[S1] = {
  //     val (newModulePath, newState) = f(modulePath, state)
  //     ModuleScope(newModulePath, newState)
  //   }

  // }

  // final case class FunctionScope[S](modulePath: ModulePath, state: S) extends Scope[S] {
  //   type ParentScope = ModuleScope[S]

  //   def map[S1](f: S => S1): Scope[S1] = FunctionScope(modulePath, f(state))
  //   def mapScope[S1](f: (ModulePath, S) => (ModulePath, S1)): FunctionScope[S1] = {
  //     val (newModulePath, newState) = f(modulePath, state)
  //     FunctionScope(newModulePath, newState)
  //   }
  // }

  // def packageScope[S](state: S): PackageScope[S]                       = PackageScope(state)
  // def moduleScope[S](modulePath: ModulePath, state: S): ModuleScope[S] = ModuleScope(modulePath, state)
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
