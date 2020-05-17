package org.morphir

import org.morphir.workspace.project.model.ProjectDir
import zio.{ Has, ZIO }

package object workspace {
  type Workspace = Has[Workspace.Service]

  def resolveProjectDir(maybeProjectDir: Option[ProjectDir]): ZIO[Workspace, Nothing, ProjectDir] =
    ZIO.accessM[Workspace](_.get.resolveProjectDir(maybeProjectDir))

}
