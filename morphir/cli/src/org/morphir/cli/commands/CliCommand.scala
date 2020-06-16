package org.morphir.cli.commands

import org.morphir.cli.{ CliAction, CliEnv }
import zio._
import zio.logging.log

abstract class CliCommand extends Product {
  def execute: ZIO[CliEnv, Nothing, ExitCode] =
    action.foldM(onError, _ => UIO.succeed(ExitCode.success))

  def action: CliAction

  def onError(error: Throwable): ZIO[CliEnv, Nothing, ExitCode] =
    for {
      _ <- log.error(s"Error encountered: $error")

    } yield ExitCode.failure

}
