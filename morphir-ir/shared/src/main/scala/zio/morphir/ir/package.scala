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

  type PackageName = PackageModule.PackageName
  val PackageName = PackageModule.PackageName

  type ??? = Nothing
}
