package org.morphir.toolbox.core

import java.nio.file.{ Path, Paths }

import org.morphir.core.newtype._
import zio._

object WorkspacePath extends Subtype[Path] {
  def of(path: String): WorkspacePath = WorkspacePath(Paths.get(path))

  def from(path: Option[Path] = None): Task[WorkspacePath] =
    Task.effect(path.getOrElse(Paths.get("."))).map(WorkspacePath(_))

  def from(path: Path): UIO[WorkspacePath] =
    ZIO.succeed(WorkspacePath(path))

  def toDirectory(path: WorkspacePath): Task[WorkspaceDir] =
    path.ifIsDirectoryM(
      WorkspaceDir.fromPath(path),
      WorkspaceDir.fromPath(path.getParent)
    )

  implicit class WorkspacePathOps(val self: WorkspacePath) extends AnyVal {
    def isDirectory: Task[Boolean] =
      Task.effect(self.toFile.isDirectory)

    def ifIsDirectoryM[R, A](
      whenTrue: => RIO[R, A],
      whenFalse: => RIO[R, A]
    ): RIO[R, A] =
      ZIO.ifM(isDirectory)(whenTrue, whenFalse)

    def workspaceManifestFile: Task[ManifestFile] =
      ManifestFile.fromPath(self)

  }
}
