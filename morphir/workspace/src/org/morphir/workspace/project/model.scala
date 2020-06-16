package org.morphir.workspace.project

import java.nio.file.{ Path, Paths }

import io.estatico.newtype.macros.newtype
import zio.{ Task, ZIO }

object model {
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

  @newtype case class ModelFilePath(toPath: Path) {
    def absolutePath: Path         = toPath.toAbsolutePath
    def toAbsolutePath: Task[Path] = ZIO.effect(absolutePath)
  }
}
