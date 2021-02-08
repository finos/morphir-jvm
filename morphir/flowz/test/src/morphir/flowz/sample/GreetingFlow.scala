package morphir.flowz.sample

import morphir.flowz.{ Step, FuncStep, StatelessStep }
import zio._
import zio.console.Console

object GreetingFlow extends App {
  final case class Target(name: String)

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    // Let's start with a behavior that gets the optional target
    val getTarget: FuncStep[List[String], Option[Target]] = Step.fromFunction { args: List[String] =>
      args.headOption.map(Target)
    }

    // Next let's construct a behavior that expects an optional target and prints a greeting to that target orF the world
    // if no target is specified
    val greeter: StatelessStep[Option[Target], Console, Nothing, Unit] = Step.stateless { greeting: Option[Target] =>
      console.putStrLn(s"Hello, ${greeting getOrElse "world"}")
    }

    val myStep = getTarget >>> greeter

    myStep.run(args).exitCode
  }
}
