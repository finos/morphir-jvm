//package morphir.flowz.sample
//
//import morphir.flowz.ContextSetup
//import morphir.flowz.api._
//import zio._
//
//object SummingFlowWithEffectfulSetup extends App {
//  def run(args: List[String]): URIO[ZEnv, ExitCode] =
//    flow(
//      "sum-flow",
//      setup = ContextSetup.uses[console.Console].extractParamsWith { args: List[String] =>
//        ZIO.collectAllSuccesses(args.map(input => ZIO.effect(input.toInt)))
//      }
//    )
//      .stages(Step.fromFunction { items: List[Int] => items.sum })
//      .build
//      .run(List("1", "2", "3", "Four", "5"))
//      .flatMap(sum => console.putStrLn(s"Sum: $sum"))
//      .exitCode
//}
