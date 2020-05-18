package org.morphir.workspace.project

import java.nio.file.{ Path, Paths }

import io.estatico.newtype.macros.newtype
import zio.{ Task, ZIO }
import scala.language.implicitConversions

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
}
