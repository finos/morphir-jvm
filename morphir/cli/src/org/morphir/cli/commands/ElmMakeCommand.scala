package org.morphir.cli.commands

import cats.implicits._
import org.morphir.cli.{ CliAction, ExitCode }
import org.morphir.workspace.project.model.{ OutputDir, ProjectDir }
import zio._
import zio.blocking.Blocking
import zio.process
import zio.logging._

final case class ElmMakeCommand(
  projectDir: Option[ProjectDir] = None,
  output: Option[OutputDir]
) extends CliCommand {

  def morphirElmProcess: ZIO[Blocking, Throwable, process.Process] =
    for {
      argsRef <- Ref.make[Array[String]](Array("make"))
      _ <- ZIO.whenCase(projectDir) {
            case Some(dir) =>
              argsRef.update(args => args ++ Array("--project-dir", dir.absolutePath.toString))
          }
      _ <- ZIO.whenCase(output) {
            case Some(filePath) =>
              argsRef.update(args => args ++ Array("--output", filePath.absolutePath.toString))
          }
      args   <- argsRef.get
      result <- process.Command("morphir-elm", args.toIndexedSeq: _*).run
    } yield result

  def action: CliAction =
    for {

      elmMake    <- morphirElmProcess
      stdoutFork <- elmMake.stdout.linesStream.foreach(line => console.putStrLn(line)).fork
      stdErrFork <- elmMake.stderr.linesStream
                     .foreach(line =>
                       log.locally(LogAnnotation.Name("ElmMakeCommand" :: Nil)) {
                         log.error(line)
                       }
                     )
                     .fork
      _ <- stdoutFork.join
      _ <- stdErrFork.join
    } yield ExitCode.Success
}

object ElmMakeCommand {

  object Cli {
    import com.monovore.decline._

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

    val makeOptions: Opts[ElmMakeCommand] =
      (projectDir, outputDir).mapN {
        case (projectDir, outputDir) =>
          ElmMakeCommand(projectDir, outputDir)
      }

    val command: Opts[ElmMakeCommand] =
      Opts.subcommand("make", help = "Compile Elm source code down to the Morphir IR")(makeOptions)
  }

}
