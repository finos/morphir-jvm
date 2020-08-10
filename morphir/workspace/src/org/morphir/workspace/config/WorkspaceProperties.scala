/*
Copyright 2020 Morgan Stanley

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/


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
