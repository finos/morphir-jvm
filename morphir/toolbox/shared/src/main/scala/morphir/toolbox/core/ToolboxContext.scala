package morphir.toolbox.core

import zio._

sealed abstract class ToolboxContext
object ToolboxContext {
  private case class UnverifiedContext(workspaceDir: WorkspaceDir) extends ToolboxContext

  def make(workspaceDir: WorkspaceDir): UIO[ToolboxContext] =
    ZIO.succeed(UnverifiedContext(workspaceDir))
}
