package morphir.flowz.sample

import morphir.flowz._
import morphir.flowz.instrumentation.InstrumentationLogging
import zio._

object HelloWorld extends App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    val helloStep = step("hello")(Step.stateless { greeting: Option[String] =>
      console.putStrLn(s"Hello, ${greeting.getOrElse("world")}")
    })

    helloStep
      .run(args.headOption)
      .provideCustomLayer(StepUidGenerator.live ++ InstrumentationLogging.console())
      .exitCode

  }
}
