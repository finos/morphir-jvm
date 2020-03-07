package morphir.cli.commands

import zio._
import zio.console
import morphir.cli._
import morphir.runtime._
import caseapp.core.RemainingArgs

object ServerCommand extends MorphirCommand[CliCommand.Server] {
  def run(
      commandLine: CliCommand.Server,
      remainingArgs: RemainingArgs
  ): Cmdlet =
    for {
      _ <- console.putStrLn("Running server command.")
    } yield ExitCode.Success
}
