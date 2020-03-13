package morphir
import zio._
import zio.blocking.Blocking
import zio.console._
import caseapp.core.app.CommandApp
import morphir.cli._
import caseapp.core.RemainingArgs
import morphir.cli
import morphir.runtime._
import zio.stream.Sink

object Main {

  def main(args: Array[String]): Unit = {
    val program =
      for {
        cmdLine <- CommandLine.make(args.toIndexedSeq)
        exitCode <- handleCommands(cmdLine).provideLayer(
          Cli.live ++ Blocking.live ++ Console.live
        )
      } yield exitCode

    Runtime.default.unsafeRun(program)
  }

  def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (for {
      cmdLine <- ZIO.effect(new CommandLine(args))

    } yield ()).fold(_ => 1, _ => 0)

  def handleCommands(commandLine: CommandLine) =
    ZIO.effectSuspend {
      commandLine.subcommands match {
        case commandLine.elm :: commandLine.elm.make :: Nil =>
          cli
            .elmMake(
              commandLine.elm.make.projectDir.toOption,
              commandLine.elm.make.output.toOption
            )
            .foreach(line => putStrLn(line))
            .orElseSucceed(ExitCode.Failure) *> UIO.succeed(ExitCode.Success)
        case commandLine.elm :: commandLine.elm.gen :: Nil =>
          putStrLn(s"morphir elm gen>") *> UIO.succeed(ExitCode.Success)
        case commandLine.generate :: subcommand :: Nil => ???
        case _ =>
          ZIO.effect(commandLine.printHelp()) *> UIO.succeed(ExitCode.Failure)
      }
    }

}
