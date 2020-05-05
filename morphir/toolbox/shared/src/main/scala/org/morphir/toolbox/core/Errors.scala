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

  final case class NoSuchBinding(
    name: String,
    message: String
  ) extends Exception(message) {
    def this(name: String) =
      this(name, s"A Binding by the name of $name was not found.")
  }

  object NoSuchBinding {
    def apply(name: String): NoSuchBinding = new NoSuchBinding(name)
  }
}
