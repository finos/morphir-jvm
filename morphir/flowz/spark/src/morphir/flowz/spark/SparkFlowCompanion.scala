package morphir.flowz.spark

import morphir.flowz.spark.sparkModule.SparkModule
import morphir.flowz.{ Flow, FlowCompanion, TaskStep }
import org.apache.spark.sql.Dataset

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

trait SparkFlowCompanion { self: FlowCompanion =>
  def mapDataset[A, B <: Product: ClassTag: TypeTag](func: A => B): TaskStep[Dataset[A], Dataset[B]] = ???
//    fromEffectful { dataSet: Dataset[A] =>
//      import dataSet.sparkSession.implicits._
//      dataSet.map(func)
//    }

  /**
   * A step that returns the given parameters.
   */
  def parameters[In]: SparkStep[Any, In, Throwable, In] =
    Flow.context[SparkModule, Any, In].mapEffect { ctx =>
      ctx.inputs.params
    }

}
