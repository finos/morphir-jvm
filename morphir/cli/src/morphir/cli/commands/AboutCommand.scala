package morphir.cli.commands

import zio._
import zio.console
import morphir.cli._
import caseapp.core.RemainingArgs

object AboutCommand extends MorphirCommand[CliCommand.About] {
  def run(commandLine: CliCommand.About, remainingArgs: RemainingArgs): Cmdlet =
    for {
      _ <- console.putStrLn("Running about command.")
    } yield ExitCode.Success
}
