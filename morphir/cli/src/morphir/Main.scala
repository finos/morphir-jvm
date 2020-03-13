package morphir
import zio._
import caseapp.core.app.CommandApp
import morphir.cli._
import caseapp.core.RemainingArgs
import morphir.runtime._
import morphir.cli.CliCommand._
import morphir.cli.commands._

object Main extends CommandApp[CliCommand] {
  override def appName: String = morphir.BuildInfo.appName
  override def appVersion: String = morphir.BuildInfo.productVersion
  override def progName: String = appName

  override def run(
      options: CliCommand,
      remainingArgs: RemainingArgs
  ): Unit = {
    val logic = options match {
      case command: Generate =>
        GenerateCommand.run(command, remainingArgs)
    }

    Runtime.default.unsafeRun(logic)
  }
}
