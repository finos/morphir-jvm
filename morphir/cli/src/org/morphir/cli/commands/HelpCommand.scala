package org.morphir.cli.commands

import com.monovore.decline.Help
import org.morphir.cli.CliAction
import zio._

case class HelpCommand(help: Help) extends CliCommand {
  def action: CliAction =
    for {
      _ <- console.putStrLn(s"$help")
    } yield ExitCode.failure
}

object HelpCommand {
  def make(help: Help): UIO[HelpCommand] = UIO.succeed(HelpCommand(help))
}
