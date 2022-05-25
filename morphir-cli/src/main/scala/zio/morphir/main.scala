package zio.morphir
import zio._
import zio.cli.CliApp
import zio.cli.HelpDoc.Span.text
import zio.Console._
import zio.morphir.cli.CliCommand
import zio.morphir.cli.CliCommand.Subcommand

object Main extends ZIOAppDefault {
  val app = CliApp.make(
    name = "morphir",
    version = "0.1.0",
    summary = text("Morphir is a command line tool for creating and running morphir IRs."),
    command = CliCommand.morphir()
  ) {
    case cmd @ CliCommand(Subcommand.Elm(Subcommand.ElmMake()), _) =>
      for {
        _ <- printLine("Running morphir elm make tooling...")
        _ <- printLine(s"Command: ${cmd}")
        _ <- printLine("TODO: Implement morphir elm make tooling")
        _ <- printLine("Done.")
      } yield ()

    case cmd @ CliCommand(Subcommand.Elm(Subcommand.ElmGen()), _) =>
      for {
        _ <- printLine("Running morphir elm gen tooling...")
        _ <- printLine(s"Command: ${cmd}")
        _ <- printLine("TODO: Implement morphir elm gen tooling")
        _ <- printLine("Done.")
      } yield ()
    case cmd @ CliCommand(CliCommand.Subcommand.Elm(_), _) =>
      for {
        _ <- printLine("Running morphir elm tooling...")
        _ <- printLine(s"Command: ${cmd}")
        _ <- printLine("TODO: Implement morphir elm tooling")
        _ <- printLine("Done.")
      } yield ()
    case cmd @ CliCommand(CliCommand.Subcommand.Make(), _) =>
      for {
        _ <- printLine("Running morphir make tooling...")
        _ <- printLine(s"Command: ${cmd}")
        _ <- printLine("TODO: Implement morphir make tooling")
        _ <- printLine("Done.")
      } yield ()
    case cmd =>
      for {
        _ <- printLine(s"Unknown command: ${cmd}")
        _ <- printLine("Run `morphir --help` for usage.")
      } yield ()
  }

  def run = for {
    args <- ZIOAppArgs.getArgs
    _    <- app.run(args.toList)
  } yield ()
}
