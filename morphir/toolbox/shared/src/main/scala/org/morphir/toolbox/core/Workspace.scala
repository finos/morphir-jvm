package org.morphir.toolbox.core

import java.io.File
import java.nio.file.Path

import org.morphir.toolbox.workspace.config.{ ProjectSettings, WorkspaceSettings }
import org.morphir.toolbox.core.ManifestFile._
import zio._
import zio.blocking.Blocking

case class Workspace(
  rootDir: WorkspaceDir,
  projectMap: Map[ProjectName, Project]
) {
  def manifestFile: Task[File] =
    rootDir
      .join(Workspace.WorkspaceFilename)
      .flatMap(p => ZIO.effect(p.toFile))

  def projects: UIO[List[Project]] = UIO.succeed(projectMap.values.toList)

}

object Workspace {
  val WorkspaceFilename = "morphir.toml"

  def fromSettings(
    settings: WorkspaceSettings
  )(implicit workspaceDir: WorkspaceDir): UIO[Workspace] =
    for {
      dir <- ZIO.succeed(workspaceDir)
      projects = settings.projects.map {
        case (key, value) =>
          val project = createProject(value, key, dir)
          project.name -> project
      }
    } yield Workspace(dir, projects)

  def load(path: Option[Path] = None): ZIO[Blocking, Throwable, Workspace] =
    for {
      workspacePath <- WorkspacePath.from(path)
      workspaceDir  <- WorkspaceDir.fromPath(workspacePath)
      manifestFile  <- ManifestFile.fromPath(workspacePath)
      settings      <- manifestFile.load
      workspace     <- fromSettings(settings)(workspaceDir)
    } yield workspace

  private[core] def createProject(
    settigs: ProjectSettings,
    name: String,
    workspaceDir: WorkspaceDir
  ): Project = {
    val projectName = ProjectName(name, settigs.name)
    val projectDir =
      settigs.projectDir getOrElse ProjectPath.of(projectName.resolvedName)
    val projectPath = ProjectPath(
      workspaceDir.joinPath(projectDir.path.toString)
    )
    Project(projectName, projectPath, List.empty, List.empty)
  }
}
