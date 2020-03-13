package morphir.cli

import java.nio.file.Path

sealed trait CliCommand

object CliCommand {
  case class ElmMake(
      projectDir: Option[Path] = None,
      output: Option[Path],
      help: Boolean = false
  ) extends CliCommand
}
