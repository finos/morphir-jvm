package morphir.flowz.spark

import org.apache.spark.sql.{ SparkSession }
import zio._

trait SparkModuleExports {
  //final type sparkModule = morphir.flowz.spark.sparkModule.type
  val sparkModule: morphir.flowz.spark.sparkModule.type = morphir.flowz.spark.sparkModule

  final type SparkModule = morphir.flowz.spark.sparkModule.SparkModule
  val SparkModule: morphir.flowz.spark.sparkModule.SparkModule.type = morphir.flowz.spark.sparkModule.SparkModule

  final type SparkStep[-StateIn, +StateOut, -Env, -Params, +Err, +Value] =
    morphir.flowz.spark.SparkStep[StateIn, StateOut, Env, Params, Err, Value]
  val SparkStep: morphir.flowz.spark.SparkStep.type = morphir.flowz.spark.SparkStep

  def sparkStep[Params, A](func: SparkSession => Params => A): SparkStep[Any, A, Any, Params, Throwable, A] =
    SparkStep.sparkStep(func)

  def sparkStepEffect[Env, Params, Err, A](
    func: SparkSession => Params => ZIO[Env with SparkModule, Err, A]
  ): SparkStep[Any, A, Nothing, Params, Err, A] = SparkStep.sparkStepEffect[Env, Params, Err, A](func)

}
