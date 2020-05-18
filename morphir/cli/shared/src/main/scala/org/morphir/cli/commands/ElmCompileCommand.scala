package org.morphir.cli.commands

import cats.data.NonEmptyList
import cats.implicits._
import io.estatico.newtype.macros.newtype
import org.morphir.cli.CliAction
import org.morphir.cli.commands.ElmCompileCommand._
import org.morphir.workspace.project.model.{ OutputDir, ProjectDir }

import scala.language.implicitConversions

case class ElmCompileCommand(
  name: PackageName,
  sourceDirectory: SourceDirectory,
  exposedModules: NonEmptyList[ModuleName],
  projectDir: Option[ProjectDir] = None,
  output: Option[OutputDir]
) extends CliCommand {
  def action: CliAction = ???
}

object ElmCompileCommand {

  @newtype case class PackageName(name: String)
  @newtype case class SourceDirectory(rawPath: String)
  @newtype case class ModuleName(name: String)

  object Cli {
    import com.monovore.decline._

    val name: Opts[PackageName] = Opts
      .option[String](
        "name",
        short = "n",
        metavar = "package-name",
        help = "The name of the package we are creating."
      )
      .map(PackageName.apply)

    val sourceDirectory: Opts[SourceDirectory] = Opts
      .option[String](
        long = "sourceDirectory",
        short = "s",
        help = "the directory (relative to the project) where sources exist"
      )
      .map(SourceDirectory.apply)

    val exposedModules: Opts[NonEmptyList[ModuleName]] =
      Opts
        .options[String]("exposed-modules", short = "e", help = "The modules exposed in the resultant package")
        .map(items => items.map(ModuleName.apply))

    val projectDir: Opts[Option[ProjectDir]] =
      Opts
        .option[String]("project-dir", short = "p", help = "The morphir project directory")
        .map(ProjectDir.apply)
        .orNone

    val outputDir: Opts[Option[OutputDir]] =
      Opts
        .option[String]("output", short = "o", help = "Target location where the Morphir IR will be sent")
        .map(OutputDir.apply)
        .orNone

    val makeOptions: Opts[ElmCompileCommand] =
      (name, sourceDirectory, exposedModules, projectDir, outputDir).mapN {
        case (name, srcDir, exposedModules, projectDir, outputDir) =>
          ElmCompileCommand(name, srcDir, exposedModules, projectDir, outputDir)
      }

    val command: Opts[ElmCompileCommand] =
      Opts.subcommand("compile", help = "Compile Elm source code down to the Morphir IR")(makeOptions)
  }

}
