package morphir.cli.commands

import morphir.cli.CliCommand
import caseapp.core.RemainingArgs
import morphir.runtime.ExitCode
import zio._
import zio.console._
import zio.nio.core.file.Path
import zio.nio.file.Files
import java.nio.charset.StandardCharsets
import java.nio.charset.Charset
import zio.blocking.`package`.Blocking

object GenerateCommand extends MorphirCommand[CliCommand.Generate] {

  def run(
      command: CliCommand.Generate,
      remaingArgs: RemainingArgs
  ): UIO[ExitCode] =
    (for {
      _ <- putStrLn(s"Command Line: $command")
      contents <- fileContents(Path.fromJava(command.input))
      _ <- putStrLn("Contents:")
      _ <- putStrLn(contents)
    } yield ExitCode.Success)
      .provideLayer(Console.live ++ Blocking.live)
      .fold(_ => ExitCode.Failure, identity)

  def fileContents(path: Path, charset: Charset = StandardCharsets.UTF_8) =
    Files
      .readAllLines(path, charset)
      .map(_.mkString(System.lineSeparator()))
}
