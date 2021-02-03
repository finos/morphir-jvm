package morphir.flowz.sample

import morphir.flowz.{ Behavior, FuncBehavior, StatelessBehavior }
import zio._
import zio.console.Console

object GreetingBehavior extends App {
  final case class Target(name: String)

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    // Let's start with a behavior that gets the optional target
    val getTarget: FuncBehavior[List[String], Option[Target]] = Behavior.fromFunction { args: List[String] =>
      args.headOption.map(Target)
    }

    // Next let's construct a behavior that expects an optional target and prints a greeting to that target orF the world
    // if no target is specified
    val greeter: StatelessBehavior[Option[Target], Console, Nothing, Unit] = Behavior.stateless {
      greeting: Option[Target] =>
        console.putStrLn(s"Hello, ${greeting getOrElse "world"}")
    }

    val myBehavior = getTarget >>> greeter

    myBehavior.trigger(args).exitCode
  }
}
