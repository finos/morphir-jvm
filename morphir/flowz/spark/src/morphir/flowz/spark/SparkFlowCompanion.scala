package morphir.flowz.spark

import morphir.flowz.{ FlowCompanion, TaskStep }
import org.apache.spark.sql.Dataset

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

trait SparkFlowCompanion { self: FlowCompanion =>
  def mapDataset[A, B <: Product: ClassTag: TypeTag](func: A => B): TaskStep[Dataset[A], Dataset[B]] =
    fromEffectful { dataSet: Dataset[A] =>
      import dataSet.sparkSession.implicits._
      dataSet.map(func)
    }
}
