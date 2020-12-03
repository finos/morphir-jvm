package morphir.flowz.spark
import morphir.flowz._
import morphir.flowz.spark.sparkModule.SparkModule
import zio._

object SparkFlow extends FlowCompanion with SparkFlowCompanion {}

object SparkStep extends FlowCompanion with SparkFlowCompanion {
  def apply[Env, In, Out](func: In => RIO[Env with SparkModule, Out]): SparkStep[Env, In, Throwable, Out] =
    Flow(ZIO.environment[(Env with SparkModule, In, Any)].flatMap { case (env, in, _) =>
      func(in).provide(env).map(out => FlowOutputs.fromOutput(out))
    })

  def input[In]: SparkStep[Any, In, Throwable, In] =
    Flow(ZIO.environment[(SparkModule, In, Any)].mapEffect { case (_, in, _) =>
      FlowOutputs.fromOutput(in)
    })

}
