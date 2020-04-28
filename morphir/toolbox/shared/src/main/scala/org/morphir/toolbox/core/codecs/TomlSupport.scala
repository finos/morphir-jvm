package org.morphir.toolbox.core.codecs

import java.nio.file.Path

import org.morphir.toolbox.core.ProjectPath
import toml._

import scala.language.implicitConversions

trait TomlSupport {

  implicit def toCodecOps[A](codec: Codec[A]): TomlCodec[A] =
    new TomlCodec[A](codec)

  implicit val pathCodec: Codec[Path] = Codec {
    case (Value.Str(value), _, _) =>
      if (value.isBlank)
        Left(List.empty -> s"A non-empty path is expected $value provided")
      else Right(Path.of(value))
    case (value, _, _) =>
      Left(List.empty -> s"A path is expected, $value provided")
  }

  implicit val projectPathCodec: Codec[ProjectPath] =
    ProjectPath.projectPathCodec

}

object TomlSupport extends TomlSupport
