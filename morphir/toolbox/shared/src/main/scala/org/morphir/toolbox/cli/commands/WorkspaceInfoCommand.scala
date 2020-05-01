package org.morphir.toolbox.cli.commands

import java.nio.file.Path

import org.morphir.toolbox.cli.{ CliCommand, CliEnv, ExitCode }
import org.morphir.toolbox.core.{ Project, SourceFile }
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
      _ <- console.putStrLn(pprint.apply(s"|-Project:").render)
      _ <- console.putStrLn(s" \\")
      _ <- console.putStrLn(s"  |-Name: ${project.name}")
      _ <- console.putStrLn(s"  |-Dir: ${project.projectDir}")
      _ <- console.putStrLn(s"  +-Sources: ")
      _ <- reportSources(project.sources)
    } yield ()

  private def reportSources(sources: Seq[SourceFile[Any]]) =
    for {
      _ <- ZStream.fromIterable(sources).foreach(reportSourceInfo)
    } yield ()

  private def reportSourceInfo(source: SourceFile[Any]): ZIO[Console, Nothing, Unit] =
    for {
      _ <- console.putStrLn(s"${source.path}")
    } yield ()

}
