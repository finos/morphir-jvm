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
