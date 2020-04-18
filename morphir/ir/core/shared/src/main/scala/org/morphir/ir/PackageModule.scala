//package org.morphir.ir
//
//import org.morphir.ir.AccessControlled.AccessControlled
//
//object PackageModule {
//  case class Declaration[X](modules: Map[Path, ModuleDeclaration[X]])
//  case class Definition[X](
//      dependencies: Map[Path, ModuleDeclaration[X]],
//      modules: Map[Path, AccessControlled[ModuleDefinition[X]]]
//  )
//
//  object Definition {
//    //implicit def rw[X](implicit extraRW: RW[X]) = macroRW[Definition[X]]
//  }
//
//  def emptyDefinition[X] =
//    Definition[X](dependencies = Map.empty, modules = Map.empty)
//}
