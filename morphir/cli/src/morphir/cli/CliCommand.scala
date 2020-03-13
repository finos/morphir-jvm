package morphir.cli

import zio._
import caseapp.core.RemainingArgs
import morphir.runtime._
import caseapp._
import java.nio.file.Path

sealed abstract class CliCommand extends Product with Serializable

object CliCommand {
  case class Generate(
      @ExtraName("i") @ValueDescription("Path to morphir model") input: Path,
      scalaOutput: Option[Path]
  ) extends CliCommand
}
