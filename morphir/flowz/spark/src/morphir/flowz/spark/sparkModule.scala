package morphir.flowz.spark

import zio._
import org.apache.spark.sql._

import scala.annotation.nowarn
import scala.reflect.runtime.universe.TypeTag

object sparkModule {
  type SparkModule = Has[SparkModule.Service]

  def createDataset[A <: Product: TypeTag](data: Seq[A]): ZIO[SparkModule, Throwable, Dataset[A]] =
    ZIO.accessM[SparkModule](_.get.createDataset(data))

  def getConfigValue(key: String): ZIO[SparkModule, NoSuchElementException, String] =
    ZIO.accessM[SparkModule](_.get.getConfigValue(key))

  def makeDataset[A](func: SparkSession => Dataset[A]): RIO[SparkModule, Dataset[A]] =
    ZIO.accessM(_.get.makeDataset(func))

  def printSchema(dataFrame: DataFrame): URIO[SparkModule, Unit] =
    ZIO.accessM(_.get.printSchema(dataFrame))

  val sparkSession: URIO[SparkModule, SparkSession] =
    ZIO.access(_.get.sparkSession)

  def withSpark[A](func: SparkSession => A): ZIO[SparkModule, Throwable, A] =
    ZIO.accessM[SparkModule](_.get.withSpark(func))

  object SparkModule {
    trait Service extends Serializable {

      /**
       * Get access to an instance of the `SparkSession`.
       */
      def sparkSession: SparkSession

      def createDataset[A <: Product: TypeTag](data: Seq[A]): Task[Dataset[A]]
      def getConfigValue(key: String): IO[NoSuchElementException, String]
      def makeDataset[A](func: SparkSession => Dataset[A]): Task[Dataset[A]]

      def printSchema(dataFrame: DataFrame): UIO[Unit]
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

        def getConfigValue(key: String): IO[NoSuchElementException, String] = Task.effect {
          sparkSession.conf.get(key)
        }.refineToOrDie[NoSuchElementException]

        def makeDataset[A](func: SparkSession => Dataset[A]): Task[Dataset[A]] = Task.effect {
          func(sparkSession)
        }

        override def printSchema(dataFrame: DataFrame): UIO[Unit] = UIO {
          dataFrame.printSchema()
        }

        def withSpark[A](func: SparkSession => A): Task[A] = Task.effect(func(sparkSession))
      }
    }

    def buildLayer(builder: SparkSession.Builder, newSession: Boolean = false): ZLayer[Any, Throwable, SparkModule] =
      ZLayer.suspend {
        val sparkSessionLayer = Task.effect {
          if (newSession) builder.getOrCreate().newSession()
          else builder.getOrCreate()
        }.toLayer
        sparkSessionLayer >>> fromSession
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
