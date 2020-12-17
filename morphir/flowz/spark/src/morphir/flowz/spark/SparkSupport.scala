package morphir.flowz.spark

import morphir.flowz.spark.SparkSupport.DatasetOps
import org.apache.spark.sql.{ Dataset, Encoder }

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

trait SparkSupport {
  implicit def toDatasetOps[A](dataset: Dataset[A]): DatasetOps[A] =
    new DatasetOps[A](dataset)
}

object SparkSupport extends SparkSupport {
  class DatasetOps[A](protected val data: Dataset[A]) {

    def splitWith[L <: Product: ClassTag: TypeTag, R <: Product: ClassTag: TypeTag](
      splitter: A => Either[L, R]
    ): (Dataset[L], Dataset[R]) = {
      import data.sparkSession.implicits._

      val items = data.map(item =>
        splitter(item) match {
          case Left(left)   => (Some(left): Option[L], None: Option[R])
          case Right(right) => (None: Option[L], Some(right): Option[R])
        }
      )

      val left  = items.flatMap(_._1)
      val right = items.flatMap(_._2)
      (left, right)
    }

    def splitWithCodec[L, R](
      splitter: A => Either[L, R]
    )(implicit
      lEncoder: Encoder[L],
      rEncoder: Encoder[R],
      ev: Encoder[(Option[L], Option[R])]
    ): (Dataset[L], Dataset[R]) = {
      val items = data.map(item =>
        splitter(item) match {
          case Left(left)   => (Some(left): Option[L], None)
          case Right(right) => (None: Option[L], Some(right): Option[R])
        }
      )

      val left  = items.flatMap(_._1)
      val right = items.flatMap(_._2)
      (left, right)
    }
  }
}
