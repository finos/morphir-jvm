package org.morphir.cli

import java.nio.file.Path

import com.monovore.decline._
import morphir.toolbox.cli.CliCommand
import morphir.toolbox.cli.commands.{ BuildCommand, WorkspaceInfoCommand }
import zio.{ IO, ZIO }

object Cli {

  def parse(args: Seq[String]): IO[Help, CliCommand] =
    ZIO.fromEither(Cli.rootCommand.parse(args))

  lazy val rootCommand: Command[CliCommand] = Command("morphir", "Morphir CLI")(
    buildCommand orElse projectCommand orElse workspaceCommand
  )

  lazy val buildCommand: Opts[BuildCommand] = Opts
    .subcommand("build", help = "Build the workspace")(
      workspaceOpt.orNone
    )
    .map(BuildCommand)

  lazy val projectCommand: Opts[CliCommand.ProjectList] = Opts.subcommand("project", help = "Work with projects")(
    Opts
      .subcommand(name = "list", help = "List projects in the workspace")(
        workspaceOpt.orNone
      )
      .map(CliCommand.ProjectList)
  )

  lazy val workspaceCommand: Opts[CliCommand] =
    Opts.subcommand("workspace", help = "Work wth workspaces") {
      val initCmd = Opts
        .subcommand("init", help = "Initialize a workspace")(
          workspaceOpt.orNone
        )
        .map(CliCommand.WorkspaceInit)

      val infoCmd = Opts
        .subcommand("info", help = "Get information about a workspace")(
          workspaceOpt.orNone
        )
        .map(WorkspaceInfoCommand)

      infoCmd orElse initCmd
    }

  private lazy val workspaceOpt = Opts.option[Path](
    "workspace",
    short = "w",
    metavar = "workspace-path",
    help = "The path to the workspace folder or manifest file."
  )

}
