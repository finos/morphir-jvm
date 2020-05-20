package org.morphir.cli.commands

import org.morphir.cli.{ CliAction, CliEnv, ExitCode }
import zio._
import zio.logging.log

abstract class CliCommand extends Product {
  def execute: ZIO[CliEnv, Nothing, ExitCode] =
    action.foldM(onError, _ => UIO.succeed(ExitCode.Success))

  def action: CliAction

  def onError(error: Throwable): ZIO[CliEnv, Nothing, ExitCode] =
    for {
      _ <- log.error(s"Error encountered: $error")

    } yield ExitCode.Failure

}
