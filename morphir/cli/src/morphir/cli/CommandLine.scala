package morphir.cli

import org.rogach.scallop._
import java.nio.file.{Path, Paths}
import upickle.default
import morphir.BuildInfo
import zio._
import org.rogach.scallop.exceptions.ScallopException

class CommandLine(arguments: Seq[String]) extends ScallopConf(arguments) {
  version(s"${BuildInfo.appName} ${BuildInfo.productVersion}")
  printedName = BuildInfo.appName
  shortSubcommandsHelp(true)
  val generate = new Subcommand("generate") {
    banner {
      """Usage: morphir generate
      |""".stripMargin
    }
    val scala = new Subcommand("scala") {
      banner {
        """Usage: morphir generate scala [Options] <model-path>
        |Options:
        |""".stripMargin
      }

      val output = opt[Path](argName = "output-path")
      val modelPath = trailArg[Path](required = true)
    }
    addSubcommand(scala)
    requireSubcommand()
  }

  addSubcommand(generate)

  val elm = new Subcommand("elm") {
    printedName = s"${BuildInfo.appName} elm"
    val make = new Subcommand("make") {
      val projectDir = opt[Path](
        argName = "path",
        descr =
          """Root directory of the project where morphir.json is located. (default: ".")""",
        default = Some(Paths.get("."))
      )
      val output = opt[Path](
        argName = "output-path",
        descr =
          "Target location where the Morphir IR will be sent. Defaults to STDOUT."
      )
    }
    addSubcommand(make)
    val gen = new Subcommand("gen")
    addSubcommand(gen)

    addValidation {
      subcommands match {
        case Nil =>
          val msg = s""" $printedName
          |${helpFormatter.formatHelp(builder, "")}
          |""".stripMargin

          Left(msg)
        case _ => Right(())
      }
    }
  }
  addSubcommand(elm)
  verify()
}

object CommandLine {
  def make(arguments: Seq[String]) = ZIO.effect(new CommandLine(arguments))

}
