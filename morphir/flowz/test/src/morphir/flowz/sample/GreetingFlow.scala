package morphir.flowz.sample

import morphir.flowz.instrumentation.InstrumentationLogging
import morphir.flowz._
import zio._
import zio.logging.LogLevel

object GreetingFlow extends App {
  final case class Target(name: String)

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    // Let's start with a behavior that gets the optional target
    val getTarget: RunnableStep[Any, Any, List[String], Any with StepRuntimeEnv, Throwable, Option[Target]] =
      step("get-target")(Step.fromFunction { args: List[String] =>
        args.headOption.map(Target)
      })

    // Next let's construct a behavior that expects an optional target and prints a greeting to that target orF the world
    // if no target is specified
    val greeter = step("greeter-step")(Step.stateless { greeting: Option[Target] =>
      console.putStrLn(s"Hello, ${greeting getOrElse "world"}")
    })

    val myStep = step("greeting")(getTarget >>> greeter)

    myStep
      .run(args)
      .provideCustomLayer(StepUidGenerator.live ++ InstrumentationLogging.console(logLevel = LogLevel.Trace))
      .exitCode
  }
}
