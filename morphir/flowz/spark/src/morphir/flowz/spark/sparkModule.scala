package morphir.flowz.spark

import zio._
import org.apache.spark.sql._

import scala.annotation.nowarn
import scala.reflect.runtime.universe.TypeTag

object sparkModule {
  type SparkModule = Has[SparkModule.Service]

  def createDataset[A <: Product: TypeTag](data: Seq[A]): ZIO[SparkModule, Throwable, Dataset[A]] =
    ZIO.accessM[SparkModule](_.get.createDataset(data))

  val sparkSession: URIO[SparkModule, SparkSession] =
    ZIO.access(_.get.sparkSession)

  def withSpark[A](func: SparkSession => A): ZIO[SparkModule, Throwable, A] =
    ZIO.accessM[SparkModule](_.get.withSpark(func))

  object SparkModule {
    trait Service extends Serializable {
      def sparkSession: SparkSession

      def createDataset[A <: Product: TypeTag](data: Seq[A]): Task[Dataset[A]]

      def withSpark[A](func: SparkSession => A): Task[A]
    }

    object Service {
      def live(sparkSession: SparkSession): Service =
        Live(sparkSession)

      def makeLive(builder: SparkSession.Builder): Task[Service] = Task.effect {
        live(builder.getOrCreate().newSession())
      }

      final case class Live(sparkSession: SparkSession) extends Service {
        def createDataset[A <: Product: TypeTag](data: Seq[A]): Task[Dataset[A]] = Task.effect {
          import sparkSession.implicits._
          sparkSession.createDataset(data)
        }

        def withSpark[A](func: SparkSession => A): Task[A] = Task.effect(func(sparkSession))
      }
    }

    @nowarn
    val fromSparkSessionBuilder: ZLayer[Has[SparkSession.Builder], Throwable, SparkModule] =
      ZLayer.fromServiceM { builder: SparkSession.Builder =>
        Service.makeLive(builder)
      }

    val fromSession: ZLayer[Has[SparkSession], Nothing, SparkModule] =
      ZLayer.fromService { spark: SparkSession =>
        Service.live(spark)
      }
  }
}
