package morphir.flowz.sample

import morphir.flowz.ContextSetup
import morphir.flowz.api.{ Step, flow }
import zio._

object SummingFlow extends App {
  def run(args: List[String]): URIO[ZEnv, ExitCode] =
    flow(
      "sum-flow",
      setup = ContextSetup.uses[console.Console].derivesParamsWith((items: List[Int]) => items)
    )
      .stages(Step.fromFunction { items: List[Int] => items.sum })
      .report(sum => console.putStrLn(s"Sum: $sum"))
      .build
      .run(List(1, 2, 3))
      .exitCode
}
