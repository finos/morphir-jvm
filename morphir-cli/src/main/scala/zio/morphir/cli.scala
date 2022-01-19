package zio.morphir
import zio.*
trait Cli {
  def selectCommand(args: List[String]): IO[CliError, CliCommand]
}

object Cli {
  def selectCommand(args: List[String]): ZIO[Cli, CliError, CliCommand] =
    ZIO.environmentWithZIO(_.get.selectCommand(args))
}

sealed trait CliCommand
sealed trait CliError extends Exception
