package org.morphir.cli

import org.morphir.cli.workspace.Workspace.ProjectDir
import zio._
package object workspace {
  type Workspace = Has[Workspace.Service]

  def resolveProjectDir(maybeProjectDir: Option[ProjectDir]): ZIO[Workspace, Nothing, ProjectDir] =
    ZIO.accessM[Workspace](_.get.resolveProjectDir(maybeProjectDir))

}
