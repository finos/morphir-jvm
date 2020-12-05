package morphir.flowz.spark
import morphir.flowz._
import morphir.flowz.spark.sparkModule.SparkModule
import zio._

object SparkFlow extends FlowCompanion with SparkFlowCompanion {}

object SparkStep extends FlowCompanion with SparkFlowCompanion {
  def apply[Env, Params, Out](func: Params => RIO[Env with SparkModule, Out]): SparkStep[Env, Params, Throwable, Out] =
    Flow.context[Env with SparkModule, Any, Params].flatMap { ctx =>
      Flow(func(ctx.inputs.params).provide(ctx.environment).map(out => OutputChannels.fromValue(out)))
    }

}
