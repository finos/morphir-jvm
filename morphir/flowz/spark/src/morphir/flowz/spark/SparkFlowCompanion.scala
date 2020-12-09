package morphir.flowz.spark

import morphir.flowz.spark.sparkModule.SparkModule
import morphir.flowz.{ Flow, FlowCompanion, FlowContext, FlowValue, OutputChannels }
import org.apache.spark.sql.{ Dataset, Encoder, SparkSession }
import zio.ZIO

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

trait SparkFlowCompanion { self: FlowCompanion =>

  def createDataset[A <: Product: ClassTag: TypeTag](
    func: SparkSession => Encoder[A] => Dataset[A]
  ): SparkStep[Any, Any, Throwable, Dataset[A]] =
    Flow(
      ZIO
        .environment[FlowContext.having.Environment[SparkModule]]
        .mapEffect { ctx =>
          val spark = ctx.environment.get.sparkSession
          FlowValue.fromValue(func(spark)(spark.implicits.newProductEncoder))
        }
    )

  def createDataset[A <: Product: ClassTag: TypeTag](
    data: => Seq[A]
  ): SparkStep[Any, Any, Throwable, Dataset[A]] =
    Flow(
      ZIO
        .environment[FlowContext.having.Environment[SparkModule]]
        .mapEffect { ctx =>
          val spark = ctx.environment.get.sparkSession
          FlowValue.fromValue(spark.createDataset(data)(spark.implicits.newProductEncoder))
        }
    )

  def environment[Env]: SparkStep[Env, Any, Nothing, Env with SparkModule] =
    Flow(
      ZIO
        .environment[FlowContext.having.Environment[Env with SparkModule]]
        .map(ctx => FlowValue.fromValue(ctx.environment))
    )

  /**
   * A step that returns the given parameters.
   */
  def parameters[In]: SparkStep[Any, In, Throwable, In] =
    Flow.context[SparkModule, Any, In].mapEffect { ctx =>
      ctx.inputs.params
    }

  def makeStep[Env, Params, Err, Out](
    func: Params => ZIO[Env with SparkModule, Err, Out]
  ): SparkStep[Env, Params, Err, Out] =
    Flow.parameters[Params].flatMap { params =>
      Flow.fromEffect(func(params))
    }

  def showDataset[A](): SparkStep[Any, Dataset[A], Throwable, Dataset[A]] =
    parameters[Dataset[A]].tapValue { dataset =>
      ZIO.effect(dataset.show())
    }

  def showDataset[A](truncate: Boolean): SparkStep[Any, Dataset[A], Throwable, Dataset[A]] =
    parameters[Dataset[A]].tapValue { dataset =>
      ZIO.effect(dataset.show(truncate))
    }

  def showDataset[A](numRows: Int, truncate: Boolean): SparkStep[Any, Dataset[A], Throwable, Dataset[A]] =
    parameters[Dataset[A]].tapValue { dataset =>
      ZIO.effect(dataset.show(numRows, truncate))
    }

  def showDataset[A](numRows: Int, truncate: Int): SparkStep[Any, Dataset[A], Throwable, Dataset[A]] =
    parameters[Dataset[A]].tapValue { dataset =>
      ZIO.effect(dataset.show(numRows, truncate))
    }

  def withSpark[A](func: SparkSession => A): SparkStep[Any, Any, Throwable, A] =
    Flow(
      ZIO
        .environment[FlowContext.having.Environment[SparkModule]]
        .mapEffect(ctx => FlowValue.fromValue(func(ctx.environment.get.sparkSession)))
    )

  def withSparkEffect[Env, Err, A](func: SparkSession => ZIO[Env, Err, A]): SparkStep[Env, Any, Err, A] =
    Flow(
      ZIO
        .environment[FlowContext.having.Environment[Env with SparkModule]]
        .flatMap(ctx => func(ctx.environment.get.sparkSession).map(OutputChannels(_)).provide(ctx.environment))
    )
}
