package org.morphir.toolbox.core

import java.nio.file.Path

import toml.{Codec, Value}

case class ProjectPath(path: Path) extends AnyVal {
  def /(segment: String): ProjectPath =
    ProjectPath.of(path.toString, segment)

}

object ProjectPath {

  @inline def of(path: String, segments: String*): ProjectPath =
    ProjectPath(Path.of(path, segments: _*))

  implicit val projectPathCodec: Codec[ProjectPath] = Codec {
    case (Value.Str(value), _, _) =>
      if (value.isBlank)
        Left(List.empty -> s"A non-empty path is expected $value provided")
      else Right(ProjectPath.of(value))
    case (value, _, _) =>
      Left(List.empty -> s"A path is expected, $value provided")
  }
}
