package zio.morphir

import zio.morphir.sdk.ResultModule

package object ir {

  type LiteralValue = Literal[Any]
  val LiteralValue: Literal.type = Literal

  type ModuleName = ModuleModule.ModuleName
  val ModuleName: ModuleModule.ModuleName.type = ModuleModule.ModuleName

  type Module = ModuleModule.type
  val Module: ModuleModule.type = ModuleModule

  type ModulePath = ModuleModule.ModulePath
  val ModulePath: ModuleModule.ModulePath.type = ModuleModule.ModulePath

  type ModuleDefinition[+Annotations] = ModuleModule.Definition[Annotations]
  val ModuleDefinition: ModuleModule.Definition.type = ModuleModule.Definition

  type ModuleSpecification[+Annotations] = ModuleModule.Specification[Annotations]
  val ModuleSpecification: ModuleModule.Specification.type = ModuleModule.Specification

  type PackageDefinition[+Annotations] = PackageModule.Definition[Annotations]
  val PackageDefinition: PackageModule.Definition.type = PackageModule.Definition

  type PackageName = PackageModule.PackageName
  val PackageName: PackageModule.PackageName.type = PackageModule.PackageName

  type PackageSpecification[+Annotations] = PackageModule.Specification[Annotations]
  val PackageSpecification: PackageModule.Specification.type = PackageModule.Specification

  type Result[+E, +A] = zio.morphir.sdk.ResultModule.Result[E, A]
  val Result: ResultModule.Result.type = zio.morphir.sdk.ResultModule.Result

  type UModuleDefinition = ModuleModule.Definition[Any]
  val UModuleDefinition: ModuleModule.Definition.type = ModuleModule.Definition

  type UModuleSpecification = ModuleModule.Specification[Any]
  val UModuleSpecification: ModuleModule.Specification.type = ModuleModule.Specification

  type UPackageDefinition = PackageModule.Definition[Any]
  val UPackageDefinition: PackageModule.Definition.type = PackageModule.Definition

  type UPackageSpecification = PackageModule.Specification[Any]
  val UPackageSpecification: PackageModule.Specification.type = PackageModule.Specification

  type ??? = Nothing
}
