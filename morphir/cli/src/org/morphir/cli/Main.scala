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

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, ExitCode] =
    program(args)
      .provideCustomLayer(env ++ Workspace.live)

  def program(args: List[String]): ZIO[CliEnv, Nothing, ExitCode] =
    for {
      cmd      <- org.morphir.cli.Cli.parse(args)
      exitCode <- cmd.execute
    } yield exitCode
}
