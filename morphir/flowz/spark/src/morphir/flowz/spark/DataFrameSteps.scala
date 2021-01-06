package morphir.flowz.spark

import morphir.flowz.FilterResult
import morphir.flowz.api.Step
import morphir.flowz.spark.sparkModule.SparkModule
import org.apache.spark.sql.{ DataFrame, Dataset, Encoder, Row }
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

object DataFrameSteps {

  /**
   * Filter a `DataFrame` keeping the included records and apply a state update function which allows you to stow
   * away the excluded records.
   */
  def filter[StateIn, StateOut, Include: ClassTag: TypeTag, Exclude: ClassTag: TypeTag](
    filterFunc: Row => FilterResult[Exclude, Include]
  )(
    updateState: (StateIn, Dataset[Exclude]) => StateOut
  )(implicit
    includeEncoder: Encoder[Include],
    excludeEncoder: Encoder[Exclude]
  ): Step[StateIn, StateOut, SparkModule, DataFrame, Throwable, Dataset[Include]] =
    Step.stage { (state: StateIn, data: DataFrame) =>
      SparkStep.withSpark { spark =>
        import spark.implicits._
        implicit val filterResultEncoder = spark.implicits.newProductEncoder[FilterResult[Exclude, Include]]
        val classified                   = data.map(filterFunc)
        val excluded                     = classified.flatMap(_.excluded)
        val included                     = classified.flatMap(_.included)
        (excluded, included)
      }.mapOutputs { case (_, (excluded, included)) =>
        val newState = updateState(state, excluded)
        (newState, included)
      }
    }
}
