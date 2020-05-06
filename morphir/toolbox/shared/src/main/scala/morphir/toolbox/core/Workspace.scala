package morphir.toolbox.core

import java.io.File
import java.nio.file.{ Path, Paths }

import morphir.toolbox.core
import morphir.toolbox.core.ManifestFile._
import morphir.toolbox.workspace.config.{ ProjectSettings, WorkspaceSettings }
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
    } yield core.Workspace(dir, projects)

  def load(path: Option[Path] = None): ZIO[Blocking, Throwable, Workspace] =
    for {
      workspacePath <- WorkspacePath.from(path)
      workspaceDir  <- WorkspaceDir.fromPath(workspacePath)
      manifestFile  <- ManifestFile.fromPath(workspacePath)
      settings      <- manifestFile.load
      workspace     <- fromSettings(settings)(workspaceDir)
    } yield workspace

  private[core] def createProject(
    settings: ProjectSettings,
    name: String,
    workspaceDir: WorkspaceDir
  ): Project = {
    val projectName = ProjectName(name, settings.name)
    val projectDir = {
      val prjPath: ProjectPath = settings.projectDir getOrElse ProjectPath.of(projectName.resolvedName)
      workspaceDir.joinPath(prjPath.path)
    }
    val projectPath = ProjectPath(projectDir)
    val bindings: Map[String, Binding] = settings.bindings.map {
      case (name, bindingSettings) =>
        name -> Binding(
          name,
          bindingSettings.srcDirs.map(dir => Paths.get(projectDir.toString, dir.toString)),
          outDir = Paths.get(projectDir.toString, bindingSettings.outDir.toString)
        )
    }
    Project(projectName, projectPath, Bindings(bindings), List.empty, List.empty)
  }
}
