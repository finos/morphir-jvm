package morphir.cli

import zio._
import caseapp.core.RemainingArgs
import morphir.runtime._

sealed abstract class CliCommand

object CliCommand {
  case class About() extends CliCommand
}
