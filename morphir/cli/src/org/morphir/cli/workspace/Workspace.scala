package org.morphir.cli.workspace

import java.nio.file.{ Path, Paths }

import io.estatico.newtype.macros.newtype
import zio._

object Workspace {
  trait Service {
    def resolveProjectDir(maybeProjectDir: Option[ProjectDir]): UIO[ProjectDir]
  }

  val live: ULayer[Workspace] = ZLayer.succeed(LiveWorkspace())

  private case class LiveWorkspace() extends Service {
    def resolveProjectDir(maybeProjectDir: Option[ProjectDir]): UIO[ProjectDir] =
      UIO.succeed(maybeProjectDir.getOrElse(ProjectDir.fromWorkingDir))
  }

  @newtype case class ProjectDir(rawPath: String) {
    def absolutePath: Path         = Paths.get(rawPath).toAbsolutePath
    def toAbsolutePath: Task[Path] = ZIO.effect(absolutePath)
  }

  object ProjectDir {

    def fromWorkingDir: ProjectDir =
      ProjectDir(Paths.get("").toAbsolutePath.toString)
  }

  @newtype case class OutputDir(rawPath: String) {
    def absolutePath: Path         = Paths.get(rawPath).toAbsolutePath
    def toAbsolutePath: Task[Path] = ZIO.effect(absolutePath)
  }
}
