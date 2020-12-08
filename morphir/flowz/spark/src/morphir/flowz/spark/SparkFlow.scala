package morphir.flowz.spark
import morphir.flowz._
import morphir.flowz.spark.sparkModule.SparkModule
import org.apache.spark.sql.Dataset
import zio._

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

object SparkFlow extends FlowCompanion with SparkFlowCompanion {
  def mapDataset[A, B <: Product: ClassTag: TypeTag](func: A => B): TaskStep[Dataset[A], Dataset[B]] =
    Flow.task { dataset: Dataset[A] =>
      import dataset.sparkSession.implicits._
      dataset.map(func)
    }

  def transformDataset[A, B <: Product: ClassTag: TypeTag, S1, S2](
    func: (S1, Dataset[A]) => (S2, Dataset[B])
  ): Flow[S1, S2, Any, Dataset[A], Throwable, Dataset[B]] =
    Flow.statefulEffect(func)
}

object SparkStep extends FlowCompanion with SparkFlowCompanion {
  def mapDataset[A, B <: Product: ClassTag: TypeTag](func: A => B): TaskStep[Dataset[A], Dataset[B]] =
    Flow.task { dataset: Dataset[A] =>
      import dataset.sparkSession.implicits._
      dataset.map(func)
    }

  def transformDataset[A, B <: Product: ClassTag: TypeTag, S1, S2](
    func: (S1, Dataset[A]) => (S2, Dataset[B])
  ): Flow[S1, S2, Any, Dataset[A], Throwable, Dataset[B]] =
    Flow.statefulEffect(func)

  def apply[Env, Params, Out](func: Params => RIO[Env with SparkModule, Out]): SparkStep[Env, Params, Throwable, Out] =
    Flow.context[Env with SparkModule, Any, Params].flatMap { ctx =>
      Flow(func(ctx.inputs.params).provide(ctx.environment).map(out => OutputChannels.fromValue(out)))
    }

}
