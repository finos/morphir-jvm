package morphir.cli

import zio._
import caseapp.core.RemainingArgs
import morphir.runtime._
import caseapp._

sealed abstract class CliCommand extends Product with Serializable

object CliCommand {
  case class About() extends CliCommand
}
