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
