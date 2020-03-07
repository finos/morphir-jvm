package morphir.cli.commands

import caseapp.core.RemainingArgs
import zio.ZIO
import morphir.cli.ExitCode
import morphir.runtime._

abstract class MorphirCommand[C] {
  def run(
      command: C,
      remaingArgs: RemainingArgs
  ): Cmdlet
}
