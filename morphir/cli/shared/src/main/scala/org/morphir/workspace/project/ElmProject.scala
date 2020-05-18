package org.morphir.workspace.project

import io.estatico.newtype.macros.newtype
import org.morphir.workspace.project.ElmProject.{ ExposedModuleNames, PackageName }
import org.morphir.workspace.config.project.SourceDirectory

import scala.language.implicitConversions

case class ElmProject(name: PackageName, sourceDirectory: SourceDirectory, exposedModules: ExposedModuleNames) {}

object ElmProject {
  @newtype case class PackageName(name: String)
  @newtype case class ModuleName(name: String)
  @newtype case class ExposedModuleNames(toList: List[ModuleName])
}
