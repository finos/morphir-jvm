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

import com.monovore.decline._
import org.morphir.cli.commands.{ CliCommand, ElmCompileCommand, ElmMakeCommand, HelpCommand, ScalaGenCommand }
import zio._

object Cli {

  def parse(args: Seq[String]): ZIO[CliEnv, Nothing, CliCommand] =
    ZIO.fromEither(rootCommand.parse(args)).fold((help: Help) => HelpCommand(help), identity)

  lazy val rootCommand: Command[CliCommand] = Command("morphir", "Morphir CLI")(
    elmCommand orElse scalaCommand
  )

  lazy val elmCommand: Opts[CliCommand] =
    Opts.subcommand("elm", help = "Access Elm tooling")(
      ElmMakeCommand.Cli.command orElse ElmCompileCommand.Cli.command
    )

  lazy val scalaCommand: Opts[CliCommand] = Opts.subcommand("scala", help = "Access Scala related tooling")(
    ScalaGenCommand.Cli.generateCommand orElse ScalaGenCommand.Cli.genCommand
  )

}
