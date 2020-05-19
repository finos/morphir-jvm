package org.morphir.workspace.config

import io.estatico.newtype.macros.newtype
import zio.config._, ConfigDescriptor._

object project {

  final case class ProjectProperties(sourceDirectory: SourceDirectory)
  object ProjectProperties {
    implicit val configDescriptor: ConfigDescriptor[ProjectProperties] =
      (string("sourceDirectory")(SourceDirectory.apply, (srcDir: SourceDirectory) => Option(srcDir.rawPath)))(
        ProjectProperties.apply,
        ProjectProperties.unapply
      )
  }

  @newtype case class SourceDirectory(rawPath: String)
  @newtype case class ProjectName(displayName: String)
  object ProjectName {
    def unapply(projectName: ProjectName): Option[String] = Option(projectName.displayName)

    implicit val configDescriptor: ConfigDescriptor[ProjectName] =
      string(ProjectName.apply, ProjectName.unapply)
  }
  @newtype case class ProjectSet(toMap: Map[String, ProjectProperties])
  object ProjectSet {
    def configDescriptor(path: String): ConfigDescriptor[ProjectSet] =
      map(path)(ProjectProperties.configDescriptor)(ProjectSet.apply, ProjectSet.unapply)

    def configDescriptor: ConfigDescriptor[ProjectSet] =
      map(ProjectProperties.configDescriptor)(ProjectSet.apply, ProjectSet.unapply)

    def unapply(arg: ProjectSet): Option[Map[String, ProjectProperties]] = Option(arg.toMap)
  }

  final case class ProjectInfo(name: ProjectName, properties: ProjectProperties)

  object ProjectInfo {
    def apply(tuple: (ProjectName, ProjectProperties)): ProjectInfo =
      ProjectInfo(tuple._1, tuple._2)

    def fromTuple(tuple: (String, ProjectProperties)): ProjectInfo =
      ProjectInfo(ProjectName(tuple._1), tuple._2)
  }
}
