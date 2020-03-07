package morphir
import zio._
import caseapp.core.app.CommandApp
import morphir.cli._
import caseapp.core.RemainingArgs
import morphir.runtime._
import morphir.cli.CliCommand._
import morphir.cli.commands._

object Main extends CommandApp[CliCommand] with MorphirRuntime {
  override def appName: String = "morphir"
  override def progName: String = appName

  override def run(
      options: CliCommand,
      remainingArgs: RemainingArgs
  ): Unit = {
    val cmdLet: Cmdlet = options match {
      case command: About =>
        AboutCommand.run(command, remainingArgs)
    }

    unsafeRun(cmdLet)
  }
}
