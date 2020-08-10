/*
Copyright 2020 Morgan Stanley

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/


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
