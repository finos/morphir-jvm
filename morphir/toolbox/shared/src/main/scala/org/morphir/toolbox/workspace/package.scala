package org.morphir.toolbox

import java.nio.file.Path
import core.Workspace
import zio._

package object workspace {
  type WorkspaceModule = Has[WorkspaceModule.Service]

  def openFrom(path: Path): RIO[WorkspaceModule, Workspace] =
    RIO.accessM[WorkspaceModule](_.get.openFrom(path))

  object WorkspaceModule {

    trait Service {
      def openFrom(path: Path): Task[Workspace]
    }

    val live: ULayer[WorkspaceModule] = ZLayer.succeed(new Live())

    class Live() extends Service {
      def openFrom(path: Path): Task[Workspace] = ???
    }

  }
}
