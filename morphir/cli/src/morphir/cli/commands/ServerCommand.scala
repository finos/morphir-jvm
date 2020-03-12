package morphir.cli.commands

import zio._
import zio.console.Console
import zio.clock.Clock
import morphir.cli._
import morphir.runtime._
import caseapp.core.RemainingArgs
import morphir.daemon.Daemon

object ServerCommand extends MorphirCommand[CliCommand.Server] {
  def run(
      commandLine: CliCommand.Server,
      remainingArgs: RemainingArgs
  ): Cmdlet =
    (
      for {
        _ <- console.putStrLn("Running server command.")
        exitCode <- daemon(commandLine.port)
      } yield exitCode
    )

  def daemon(port: Int): Cmdlet =
    Daemon.live(port).fold(_ => ExitCode.Failure, _ => ExitCode.Success)
}
