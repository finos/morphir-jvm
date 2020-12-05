/*
Copyright 2020 Morgan Stanley

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

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
