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
import java.nio.file.Path

import org.morphir.cli.CliAction
import org.morphir.workspace.project.model.ModelFilePath
import zio.logging._

final case class ScalaGenCommand(modelFile: ModelFilePath) extends CliCommand {
  def action: CliAction =
    log
      .locally(LogAnnotation.Name(getClass.getName :: Nil)) {
        log.info(s"model-file: $modelFile")
      }
      .exitCode
}

object ScalaGenCommand {
  object Cli {
    import com.monovore.decline._

    val modelPath: Opts[ModelFilePath] =
      Opts
        .argument[Path]("model-file")
        .map(ModelFilePath.apply)

    val genOptions: Opts[ScalaGenCommand] = modelPath.map(ScalaGenCommand(_))

    val genCommand: Opts[ScalaGenCommand] =
      Opts.subcommand("gen", help = "Generate Scala source code from a morphir model")(genOptions)

    val generateCommand: Opts[ScalaGenCommand] =
      Opts.subcommand("generate", help = "Generate Scala source code from a morphir model")(genOptions)
  }
}
