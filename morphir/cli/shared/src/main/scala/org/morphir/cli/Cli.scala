package org.morphir.cli

import com.monovore.decline._
import org.morphir.cli.commands.{ CliCommand, ElmCompileCommand, ElmMakeCommand, HelpCommand }
import zio._

object Cli {

  def parse(args: Seq[String]): ZIO[CliEnv, Nothing, CliCommand] =
    ZIO.fromEither(Cli.rootCommand.parse(args)).fold((help: Help) => HelpCommand(help), identity)

  lazy val rootCommand: Command[CliCommand] = Command("morphir", "Morphir CLI")(
    elmCommand
  )

  lazy val elmCommand: Opts[CliCommand] =
    Opts.subcommand("elm", help = "Access Elm tooling")(
      ElmMakeCommand.Cli.command orElse ElmCompileCommand.Cli.command
    )

}
