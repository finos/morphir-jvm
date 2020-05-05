package org.morphir.toolbox.cli.commands

import java.nio.file.Path

import fansi.Color
import org.morphir.toolbox.cli.{ CliCommand, CliEnv, ExitCode }
import org.morphir.toolbox.core.{ Binding, Bindings, Project }
import org.morphir.toolbox.workspace
import zio._
import zio.console.Console
import zio.stream.ZStream

final case class WorkspaceInfoCommand(workspacePath: Option[Path]) extends CliCommand {
  val execute: ZIO[CliEnv, Nothing, ExitCode] = (for {
    theWorkspace <- workspace.openFrom(workspacePath)
    projects     <- theWorkspace.projects
    _            <- reportProjectInfo(projects)
  } yield ExitCode.Success).catchAll { error =>
    for {
      _ <- console.putStrLn(s"Error enountered: $error")
    } yield ExitCode.Failure
  }

  private def reportProjectInfo(projects: Seq[Project]): ZIO[Console, Option[Nothing], Unit] =
    for {
      _ <- console.putStrLn("=========================================================================")
      _ <- ZStream.fromIterable(projects).foreach(reportProjectInfo)
      _ <- console.putStrLn("=========================================================================")
    } yield ()

  private def reportProjectInfo(project: Project): ZIO[Console, Nothing, Unit] =
    for {
      _ <- console.putStrLn(s"|-${Color.Green("Project")}:")
      _ <- console.putStrLn(s" \\")
      _ <- console.putStrLn(s"  |-${Color.Yellow("Name")}: ${Color.Blue(project.name.toString)}")
      _ <- console.putStrLn(s"  |-${Color.Yellow("Dir")}: ${Color.Blue(project.projectDir.toString)}")
      _ <- console.putStrLn(s"  +-${Color.Yellow("Bindings")}: ")
      _ <- reportBindings(project.bindings)
    } yield ()

  private def reportBindings(bindings: Bindings) =
    ZStream.fromIterable(bindings.value.values).foreach(reportBinding)

  private def reportBinding(binding: Binding) =
    for {
      _ <- console.putStrLn(s"    |-${Color.Yellow("Binding")}:")
      _ <- console.putStrLn(s"      \\-${Color.Yellow("Name")}: ${Color.Blue(binding.name.toString)}")
      _ <- console.putStrLn(s"      |-${Color.Yellow("Sources")}:")
      _ <- reportSources(binding.srcDirs)
    } yield ()

  private def reportSources(sources: Seq[Path]) =
    for {
      _ <- ZStream.fromIterable(sources).foreach(reportSourceInfo)
    } yield ()

  private def reportSourceInfo(sourceDir: Path): ZIO[Console, Nothing, Unit] =
    for {
      _ <- console.putStrLn(s"        - ${Color.Blue(sourceDir.toAbsolutePath.toString)}")
    } yield ()

}
