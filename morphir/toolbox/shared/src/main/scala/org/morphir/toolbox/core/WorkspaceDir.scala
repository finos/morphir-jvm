package org.morphir.toolbox.core

import java.nio.file.Path

import zio._

final case class WorkspaceDir private (path: Path) extends AnyVal {
  def /[R](segment: String): RIO[R, WorkspaceDir] =
    RIO.effect(Path.of(path.toString, segment).toFile).flatMap { file =>
      if (file.isDirectory) Task.effect(WorkspaceDir(file.toPath))
      else
        RIO.fail(
          Errors.PathIsNotADirectoryError(
            file.toPath,
            s"The workspace directory must point at a directory, but $file is not a directory."
          )
        )
    }

  def join(segments: String*): UIO[Path] =
    ZIO.succeed(Path.of(path.toString, segments: _*))

  def joinPath(segments: String*): Path =
    Path.of(path.toString, segments: _*)

}

object WorkspaceDir {
  def fromPath[R](path: Path): RIO[R, WorkspaceDir] =
    RIO.ifM(Task.effect(path.toFile.isDirectory))(
      RIO.effect(WorkspaceDir(path)),
      RIO.fail(
        Errors.PathIsNotADirectoryError(
          path,
          s"The workspace directory must point at a directory, but $path is not a directory."
        )
      )
    )
}
