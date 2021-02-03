package morphir.flowz.sample

import morphir.flowz.Behavior
import zio._

object HelloWorld extends App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    val helloBehavior = Behavior.stateless { greeting: Option[String] =>
      console.putStrLn(s"Hello, ${greeting.getOrElse("world")}")
    }

    helloBehavior.trigger(args.headOption).exitCode

  }
}
