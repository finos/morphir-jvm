package org.morphir.cli.commands
import java.nio.file.Path

import org.morphir.cli.{ CliAction, ExitCode }
import org.morphir.workspace.project.model.ModelFilePath
import zio.logging._

final case class ScalaGenCommand(modelFile: ModelFilePath) extends CliCommand {
  def action: CliAction =
    (log
      .locally(LogAnnotation.Name(getClass.getName :: Nil)) {
        log.info(s"model-file: $modelFile")
      })
      .as(ExitCode.Success)
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
