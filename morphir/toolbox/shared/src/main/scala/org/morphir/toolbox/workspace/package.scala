package org.morphir.toolbox

import java.nio.file.Path

import core.Workspace
import zio._
import zio.blocking.Blocking

package object workspace {
  type WorkspaceModule = Has[WorkspaceModule.Service]

  def openFrom(path: Option[Path] = None): RIO[WorkspaceModule, Workspace] =
    RIO.accessM[WorkspaceModule](_.get.openFrom(path))

  object WorkspaceModule {

    trait Service {
      def openFrom(path: Option[Path] = None): Task[Workspace]
    }

    val live: RLayer[Blocking, WorkspaceModule] = ZLayer.fromFunction(env => new Live(env))

    class Live(blocking: Blocking) extends Service {
      def openFrom(path: Option[Path] = None): Task[Workspace] = Workspace.load(path).provide(blocking)
    }

  }
}
