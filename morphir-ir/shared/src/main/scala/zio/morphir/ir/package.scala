package zio.morphir

package object ir {
  type FQName = naming.FQName
  val FQName = naming.FQName

  type LiteralValue = Literal[Nothing]
  val LiteralValue = Literal

  type ModuleName = naming.ModuleName
  val ModuleName = naming.ModuleName

  type ModulePath = naming.ModulePath
  val ModulePath = naming.ModulePath

  type Name = naming.Name
  val Name = naming.Name

  type PackageName = naming.PackageName
  val PackageName = naming.PackageName

  type Path = naming.Path
  val Path = naming.Path

  type ??? = Nothing
}
