package morphir.flowz.sample

import morphir.flowz.api.{ Step, StepContext, flow }
import zio._

object SummingFlow extends App {
  def run(args: List[String]): URIO[ZEnv, ExitCode] =
    flow("sum-flow")
      .setup((items: List[Int]) => StepContext.fromParams[List[Int]](items))
      .stages(Step.fromFunction { items: List[Int] => items.sum })
      .build
      .run(List(1, 2, 3))
      .flatMap(sum => console.putStrLn(s"Sum: $sum"))
      .exitCode
}
