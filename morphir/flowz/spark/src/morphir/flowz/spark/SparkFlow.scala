package morphir.flowz.spark
import morphir.flowz._
import org.apache.spark.sql.Dataset

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
