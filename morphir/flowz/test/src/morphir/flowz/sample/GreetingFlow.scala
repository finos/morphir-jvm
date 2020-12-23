package morphir.flowz.sample

import morphir.flowz.api.Step
import zio._

object GreetingFlow extends App {
  final case class Target(name: String)

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    // Let's start with a step that gets the optional target
    val getTarget = Step.fromFunction { args: List[String] => args.headOption.map(Target) }

    // Next let's construct a step that expects an optional target and prints a greeting to that target or the world
    // if no target is specified
    val greeterStep = Step.fromEffect { greeting: Option[Target] =>
      console.putStrLn(s"Hello, ${greeting getOrElse "world"}")
    }

    val myFlow = getTarget >>> greeterStep

    myFlow.run(args).exitCode
  }
}
