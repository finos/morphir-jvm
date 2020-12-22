package morphir.flowz.sample

import morphir.flowz.api._
import zio._

object HelloWorld extends App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    val helloStep = Step.fromEffect { greeting: Option[String] =>
      console.putStrLn(s"Hello, ${greeting.getOrElse("world")}")
    }

    helloStep.run(args.headOption).exitCode

  }
}
