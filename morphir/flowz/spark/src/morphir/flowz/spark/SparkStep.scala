package morphir.flowz.spark

import morphir.flowz.spark.sparkModule.SparkModule
import morphir.flowz.{ Step, StepContext, StepOutputs }
import org.apache.spark.broadcast.Broadcast
import org.apache.spark.sql._
import zio._

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

object SparkStep {

  def apply[StateIn, StateOut, Env, Params, Err, Value](
    effect: ZIO[StepContext[Env with SparkModule, StateIn, Params], Err, StepOutputs[StateOut, Value]],
    name: Option[String] = None,
    description: Option[String] = None
  ): SparkStep[StateIn, StateOut, Env, Params, Err, Value] =
    Step[StateIn, StateOut, Env with SparkModule, Params, Err, Value](
      rawEffect = effect,
      name = name,
      description = description
    )

  def apply[Env, Params, Err, Out](
    func: Params => ZIO[Env with SparkModule, Err, Out]
  ): Step[Any, Unit, Env with SparkModule, Params, Err, Out] =
    Step.context[Env with SparkModule, Any, Params].flatMap { ctx =>
      Step(func(ctx.inputs.params).provide(ctx.environment).map(out => StepOutputs.fromValue(out)))
    }

  def broadcast[State, Params, A: ClassTag](
    func: (SparkSession, State, Params) => A
  ): Step[State, State, SparkModule, Params, Throwable, Broadcast[A]] =
    Step.context[SparkModule, State, Params].transformEff { case (_, ctx) =>
      val spark     = ctx.environment.get.sparkSession
      val value     = func(spark, ctx.inputs.state, ctx.inputs.params)
      val broadcast = spark.sparkContext.broadcast(value)
      (ctx.inputs.state, broadcast)
    }

  def createDataset[A <: Product: ClassTag: TypeTag](
    func: SparkSession => Encoder[A] => Dataset[A]
  ): SparkStep[Any, Dataset[A], Any, Any, Throwable, Dataset[A]] =
    SparkStep(
      ZIO
        .environment[StepContext.having.Environment[SparkModule]]
        .mapEffect { ctx =>
          val spark = ctx.environment.get.sparkSession
          StepOutputs.setBoth(func(spark)(spark.implicits.newProductEncoder))
        }
    )

  def createDataset[A <: Product: ClassTag: TypeTag](
    data: => Seq[A]
  ): SparkStep[Any, Dataset[A], Any, Any, Throwable, Dataset[A]] =
    SparkStep(
      ZIO
        .environment[StepContext.having.Environment[SparkModule]]
        .mapEffect { ctx =>
          val spark = ctx.environment.get.sparkSession
          StepOutputs.setBoth(spark.createDataset(data)(spark.implicits.newProductEncoder))
        }
    )

  def environment[Env]: SparkStep[Any, Env, Nothing, Any, Nothing, Env with SparkModule] =
    SparkStep[Any, Env, Nothing, Any, Nothing, Env with SparkModule](
      ZIO
        .environment[StepContext.having.Environment[Env with SparkModule]]
        .map(ctx => StepOutputs.setBoth(ctx.environment))
    )

  def makeStep[Env, Params, Err, Out](
    func: Params => ZIO[Env with SparkModule, Err, Out]
  ): Step[Any, Unit, Env with SparkModule, Params, Err, Out] =
    Step.parameters[Params].flatMap { params =>
      Step.fromEffect(func(params))
    }

  def mapDataset[A, B <: Product: ClassTag: TypeTag](
    func: A => B
  ): Step[Any, Dataset[A], Any, Dataset[A], Throwable, Dataset[B]] =
    Step.parameters[Dataset[A]].mapEffect { dataset: Dataset[A] =>
      import dataset.sparkSession.implicits._
      dataset.map(func)
    }

  def parameters[Params]: SparkStep[Any, Params, Any, Params, Nothing, Params] =
    SparkStep(ZIO.environment[StepContext[SparkModule, Any, Params]].map(ctx => StepOutputs.setBoth(ctx.inputs.params)))

  def showDataset[A](): Step[Any, Dataset[A], Any, Dataset[A], Throwable, Dataset[A]] =
    Step.parameters[Dataset[A]].tapValue { dataset =>
      ZIO.effect(dataset.show())
    }

  def showDataset[A](truncate: Boolean): Step[Any, Dataset[A], Any, Dataset[A], Throwable, Dataset[A]] =
    Step.parameters[Dataset[A]].tapValue { dataset =>
      ZIO.effect(dataset.show(truncate))
    }

  def showDataset[A](numRows: Int, truncate: Boolean): Step[Any, Dataset[A], Any, Dataset[A], Throwable, Dataset[A]] =
    Step.parameters[Dataset[A]].tapValue { dataset =>
      ZIO.effect(dataset.show(numRows, truncate))
    }

  def showDataset[A](numRows: Int, truncate: Int): Step[Any, Dataset[A], Any, Dataset[A], Throwable, Dataset[A]] =
    Step.parameters[Dataset[A]].tapValue { dataset =>
      ZIO.effect(dataset.show(numRows, truncate))
    }

  val sparkSession: Step[Any, SparkSession, SparkModule, Any, Throwable, SparkSession] =
    Step.environment[SparkModule].transformEff { (_, sparkMod) =>
      val sparkSession = sparkMod.get.sparkSession
      (sparkSession, sparkSession)
    }

  def sparkStep[Params, A](func: SparkSession => Params => A): SparkStep[Any, A, Any, Params, Throwable, A] =
    SparkStep(
      ZIO
        .environment[StepContext[SparkModule, Any, Params]]
        .mapEffect(ctx => StepOutputs.setBoth(func(ctx.environment.get.sparkSession)(ctx.inputs.params)))
    )

  def sparkStepEffect[Env, Params, Err, A](
    func: SparkSession => Params => ZIO[Env with SparkModule, Err, A]
  ): SparkStep[Any, A, Nothing, Params, Err, A] =
    SparkStep[Any, A, Env, Params, Err, A](
      ZIO
        .environment[StepContext[Env with SparkModule, Any, Params]]
        .flatMap(ctx =>
          func(ctx.environment.get.sparkSession)(ctx.inputs.params)
            .flatMap(value => ZIO.succeed(StepOutputs.unified(value)))
            .provide(ctx.environment)
        )
    )

  def state[State]: SparkStep[State, State, Any, Any, Nothing, State] = SparkStep(
    ZIO.access[StepContext[SparkModule, State, Any]](ctx => StepOutputs.setBoth(ctx.inputs.state))
  )

  def stateM[StateIn, StateOut, Env, Params, Err, Value](
    func: StateIn => Step[Any, StateOut, Env with SparkModule, Params, Err, Value]
  ): SparkStep[StateIn, StateOut, Env, Params, Err, Value] = SparkStep[StateIn, StateOut, Env, Params, Err, Value](
    ZIO.accessM[StepContext[Env with SparkModule, StateIn, Params]](ctx => func(ctx.inputs.state).effect)
  )

  def toDataFrame[A]: SparkStep[Any, Unit, Any, Dataset[A], Throwable, DataFrame] = SparkStep { data: Dataset[A] =>
    ZIO.effect(data.toDF())
  }

  def transformDataset[A, B <: Product: ClassTag: TypeTag, S1, S2](
    func: (S1, Dataset[A]) => (S2, Dataset[B])
  ): Step[S1, S2, Any, Dataset[A], Throwable, Dataset[B]] =
    Step.statefulEffect(func)

  def withSpark[A](func: SparkSession => A): SparkStep[Any, A, Any, Any, Throwable, A] =
    SparkStep(
      ZIO
        .environment[StepContext.having.Environment[SparkModule]]
        .mapEffect(ctx => StepOutputs.setBoth(func(ctx.environment.get.sparkSession)))
    )

  def withSparkEffect[Env, Err, A](
    func: SparkSession => ZIO[Env, Err, A]
  ): SparkStep[Any, A, Env, Any, Err, A] =
    SparkStep[Any, A, Env, Any, Err, A](
      ZIO
        .environment[StepContext.having.Environment[Env with SparkModule]]
        .flatMap(ctx => func(ctx.environment.get.sparkSession).map(StepOutputs.unified(_)).provide(ctx.environment))
    )
}
