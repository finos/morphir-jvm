package morphir.flowz.sample

import morphir.flowz.DefaultRunnableFlow
import zio.IO

object BasicFlow extends DefaultRunnableFlow {
  type Params                  = MyParameters
  type CommandLineParsingError = Throwable

  def parseCommandLine(args: List[String]): IO[CommandLineParsingError, Params] = args match {
    case Nil                     => IO.fail(new IllegalArgumentException("Received empty command line, please spec"))
    case greeting :: Nil         => IO.succeed(MyParameters(greeting = greeting, target = "World"))
    case greeting :: target :: _ => IO.succeed(MyParameters(greeting = greeting, target = target))
  }

  final case class MyParameters(greeting: String, target: String)

}
