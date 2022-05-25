package zio.morphir.cli
import zio.cli.{Command, Options}

final case class CliCommand[+CommandDetails](subcommand: CommandDetails, verbosity: Verbosity = Verbosity.Normal)

object CliCommand {
  sealed trait Subcommand    extends Product with Serializable
  sealed trait ElmSubcommand extends Subcommand
  object Subcommand {
    final case class Elm(elmSubcommand: ElmSubcommand) extends Subcommand
    final case class ElmMake()                         extends ElmSubcommand
    final case class ElmGen()                          extends ElmSubcommand
    final case class Make()                            extends Subcommand
  }

  lazy val verbosityFlag = Options
    .enumeration("verbosity")("quiet" -> Verbosity.Quiet, "normal" -> Verbosity.Normal, "verbose" -> Verbosity.Verbose)
    .alias("v")
    .withDefault(Verbosity.Normal)
  lazy val elm: Command[CliCommand[Subcommand.Elm]] =
    Command("elm").subcommands(elmMake, elmGen).map(cmd => CliCommand(Subcommand.Elm(cmd.subcommand), cmd.verbosity))
  private lazy val elmMake: Command[CliCommand[Subcommand.ElmMake]] = Command("make", verbosityFlag).map { v =>
    CliCommand(Subcommand.ElmMake(), v)
  }
  private lazy val elmGen: Command[CliCommand[Subcommand.ElmGen]] = Command("gen", verbosityFlag).map { v =>
    CliCommand(Subcommand.ElmGen(), v)
  }
  lazy val make = Command("make", verbosityFlag).map(v => CliCommand(Subcommand.Make(), v))

  def morphir(commandName: String = "morphir") = Command(name = commandName).subcommands(elm, make)
}

sealed trait Verbosity extends Product with Serializable
object Verbosity {
  case object Quiet   extends Verbosity
  case object Normal  extends Verbosity
  case object Verbose extends Verbosity
}
