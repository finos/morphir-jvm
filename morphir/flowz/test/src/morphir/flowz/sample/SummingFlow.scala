package morphir.flowz.sample

import morphir.flowz.api._
import zio._

object SummingFlow extends App {
  def run(args: List[String]): URIO[ZEnv, ExitCode] =
    flow("sum-flow")
      .setup(StepContext.fromParams[List[Int]])
      .stages(Step.fromFunction { items: List[Int] => items.sum })
      .build
      .run(List(1, 2, 3))
      .exitCode
}
