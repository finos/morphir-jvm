package org.morphir.workspace.config
import org.morphir.workspace.config.project.{ ProjectInfo, ProjectSet }
import zio.config._

final case class WorkspaceProperties(projects: ProjectSet) {
  lazy val projectList: List[ProjectInfo] = projects.toMap.map(ProjectInfo.fromTuple).toList
}

object WorkspaceProperties {
  val config: ConfigDescriptor[WorkspaceProperties] =
    ProjectSet.configDescriptor("projects")(
      WorkspaceProperties.apply,
      WorkspaceProperties.unapply
    )
}
