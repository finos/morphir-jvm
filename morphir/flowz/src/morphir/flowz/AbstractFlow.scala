package morphir.flowz

import zio._

abstract class AbstractFlow {
  type Params
  type CommandLineParsingError

  def parseCommandLine(args: List[String]): IO[CommandLineParsingError, Params]

  def configure(params: Params)

  def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    (for {
      params <- parseCommandLine((args))
    } yield params).foldM(
      error => console.putStrLn(s"Error occurred: $error") *> ZIO.succeed(ExitCode.failure),
      params => console.putStrLn(s"Parsed command line params to: $params") *> ZIO.succeed(ExitCode.success)
    )
}

abstract class DefaultRunnableFlow extends AbstractFlow with App {}
