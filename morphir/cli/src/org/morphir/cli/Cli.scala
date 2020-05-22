package org.morphir.cli

import com.monovore.decline._
import org.morphir.cli.commands.{ CliCommand, ElmCompileCommand, ElmMakeCommand, HelpCommand, ScalaGenCommand }
import zio._

object Cli {

  def parse(args: Seq[String]): ZIO[CliEnv, Nothing, CliCommand] =
    ZIO.fromEither(Cli.rootCommand.parse(args)).fold((help: Help) => HelpCommand(help), identity)

  lazy val rootCommand: Command[CliCommand] = Command("morphir", "Morphir CLI")(
    elmCommand orElse scalaCommand
  )

  lazy val elmCommand: Opts[CliCommand] =
    Opts.subcommand("elm", help = "Access Elm tooling")(
      ElmMakeCommand.Cli.command orElse ElmCompileCommand.Cli.command
    )

  lazy val scalaCommand: Opts[CliCommand] = Opts.subcommand("scala", help = "Access Scala related tooling")(
    ScalaGenCommand.Cli.generateCommand orElse ScalaGenCommand.Cli.genCommand
  )

}
