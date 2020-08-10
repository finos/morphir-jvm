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


package org.morphir.workspace

import zio._

import org.morphir.workspace.project.model.ProjectDir

object Workspace {
  trait Service {
    def resolveProjectDir(maybeProjectDir: Option[ProjectDir]): UIO[ProjectDir]
  }

  val live: ULayer[Workspace] = ZLayer.succeed(LiveWorkspace())

  private case class LiveWorkspace() extends Service {
    def resolveProjectDir(maybeProjectDir: Option[ProjectDir]): UIO[ProjectDir] =
      UIO.succeed(maybeProjectDir.getOrElse(ProjectDir.fromWorkingDir))
  }

}
