package zio.morphir

package object ir {

  type LiteralValue = Literal[Any]
  val LiteralValue = Literal

  type ModuleName = ModuleModule.ModuleName
  val ModuleName = ModuleModule.ModuleName

  type Module = ModuleModule.type
  val Module = ModuleModule

  type ModulePath = ModuleModule.ModulePath
  val ModulePath = ModuleModule.ModulePath

  type ModuleDefinition[+Annotations] = ModuleModule.Definition[Annotations]
  val ModuleDefinition = ModuleModule.Definition

  type ModuleSpecification[+Annotations] = ModuleModule.Specification[Annotations]
  val ModuleSpecification = ModuleModule.Specification

  type PackageDefinition[+Annotations] = PackageModule.Definition[Annotations]
  val PackageDefinition = PackageModule.Definition

  type PackageName = PackageModule.PackageName
  val PackageName = PackageModule.PackageName

  type PackageSpecification[+Annotations] = PackageModule.Specification[Annotations]
  val PackageSpecification = PackageModule.Specification

  type Result[+E, +A] = zio.morphir.sdk.ResultModule.Result[E, A]
  val Result = zio.morphir.sdk.ResultModule.Result

  type UModuleDefinition = ModuleModule.Definition[Any]
  val UModuleDefinition = ModuleModule.Definition

  type UModuleSpecification = ModuleModule.Specification[Any]
  val UModuleSpecification = ModuleModule.Specification

  type UPackageDefinition = PackageModule.Definition[Any]
  val UPackageDefinition = PackageModule.Definition

  type UPackageSpecification = PackageModule.Specification[Any]
  val UPackageSpecification = PackageModule.Specification

  type UType = TypeModule.UType
  val UType = TypeModule.UType

  type ??? = Nothing
}
