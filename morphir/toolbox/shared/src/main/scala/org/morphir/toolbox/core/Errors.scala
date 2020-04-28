package org.morphir.toolbox.core

import java.io.IOException
import java.nio.file.Path

import toml.Parse

object Errors {

  final case class PathIsNotADirectoryError(
      path: Path,
      message: String,
      cause: Option[IOException] = None
  ) extends IOException(message, cause.orNull)

  final case class ManifestFileParseError(
      details: Parse.Error,
      message: String = "Failed to parse morphir workspace manifest file."
  ) extends Exception(message)

}
