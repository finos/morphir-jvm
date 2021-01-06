package morphir.flowz.sample

import morphir.flowz.api._
import zio._

object SummingFlowWithEffectfulSetup extends App {
  def run(args: List[String]): URIO[ZEnv, ExitCode] =
    flow("sum-flow").setupWithEffect { args: List[String] =>
      val parsedItems = args.map(input => ZIO.effect(input.toInt))
      ZIO
        .collectAllSuccesses(parsedItems)
        .map(StepContext.fromParams[List[Int]])
    }
      .stages(Step.fromFunction { items: List[Int] => items.sum })
      .build
      .run(List("1", "2", "3", "Four", "5"))
      .flatMap(sum => console.putStrLn(s"Sum: $sum"))
      .exitCode
}
