package org.morphir.toolbox.cli

import java.nio.file.Path

import zio._

abstract class CliCommand extends Product {
  def execute: ZIO[CliEnv, Nothing, ExitCode]
}

object CliCommand {

  final case class Build(workspacePath: Option[Path]) extends CliCommand {
    val execute: ZIO[CliEnv, Nothing, ExitCode] =
      console.putStrLn(s"Executed: $this") *> ExitCode.success
  }
  final case class ProjectList(workspacePath: Option[Path]) extends CliCommand {
    val execute: ZIO[CliEnv, Nothing, ExitCode] =
      console.putStrLn(s"Executed: $this") *> ExitCode.success
  }

  final case class WorkspaceInit(workspacePath: Option[Path]) extends CliCommand {
    val execute: ZIO[CliEnv, Nothing, ExitCode] =
      console.putStrLn(s"Executed: $this") *> ExitCode.success
  }

}
