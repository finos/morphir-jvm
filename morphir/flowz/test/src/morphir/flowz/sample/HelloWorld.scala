package morphir.flowz.sample

import morphir.flowz.Step
import zio._

object HelloWorld extends App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    val helloBehavior = Step.stateless { greeting: Option[String] =>
      console.putStrLn(s"Hello, ${greeting.getOrElse("world")}")
    }

    helloBehavior.run(args.headOption).exitCode

  }
}
