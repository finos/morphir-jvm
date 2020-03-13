package morphir.ir

package object advanced {
  type UpdateFieldsList[X] = List[(Name, Value[X])]
  type PatternMatchCasesList[X] = List[(Pattern[X], Value[X])]

  type ModuleDeclaration[X] = Module.Declaration[X]
  val ModuleDeclaration = Module.Declaration

  type ModuleDefinition[X] = Module.Definition[X]
  val ModuleDefinition = Module.Definition

  type Package = PackageModule.type
  val Package = PackageModule

  type PackageDeclaration[X] = Package.Declaration[X]
  type PackageDefinition[X] = Package.Definition[X]

}
