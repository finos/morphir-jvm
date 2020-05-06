package morphir.toolbox.cli.commands

import java.nio.file.Path

import morphir.toolbox.cli.{ CliCommand, CliEnv, ExitCode }
import morphir.toolbox.workspace
import zio.{ console, ZIO }

final case class BuildCommand(workspacePath: Option[Path]) extends CliCommand {
  override def execute: ZIO[CliEnv, Nothing, ExitCode] =
    (for {
      theWorkspace <- workspace.openFrom(workspacePath)
      _            <- theWorkspace.projects
    } yield ExitCode.Success).catchAll { error =>
      for {
        _ <- console.putStrLn(s"Error enountered: $error")
      } yield ExitCode.Failure
    }
}
