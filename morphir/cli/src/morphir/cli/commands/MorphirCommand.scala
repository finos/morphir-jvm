package morphir.cli.commands

import caseapp.core.RemainingArgs
import zio.UIO
import morphir.runtime._

abstract class MorphirCommand[C] {
  def run(
      command: C,
      remaingArgs: RemainingArgs
  ): UIO[ExitCode]
}
