package org.morphir.cli

import org.morphir.workspace._
import zio._
import zio.clock.Clock
import zio.console.Console
import zio.logging._

object Main extends App {
  type CliEnv = zio.ZEnv with Logging with Workspace

  val env: ZLayer[Console with Clock, Nothing, Logging] =
    Logging.console(
      format = (_, logEntry) => logEntry,
      rootLoggerName = Some("morphir")
    ) ++ Workspace.live

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    program(args)
      .provideCustomLayer(env ++ Workspace.live)

  def program(args: List[String]): ZIO[CliEnv, Nothing, Int] =
    (for {
      cmd      <- Cli.parse(args)
      exitCode <- cmd.execute
      ec       = exitCode.code
    } yield ec) *> ZIO.succeed(1)
}
