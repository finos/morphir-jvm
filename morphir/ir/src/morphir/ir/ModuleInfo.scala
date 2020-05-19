package morphir.ir

final case class ModuleInfo[+A](path: ModulePath, definition: ModuleDefinition[A]) {
  def toTuple: (ModulePath, ModuleDefinition[A]) = path -> definition
}
object ModuleInfo {

  def apply[A](args: (ModulePath, ModuleDefinition[A])): ModuleInfo[A] =
    ModuleInfo(args._1, args._2)
}
