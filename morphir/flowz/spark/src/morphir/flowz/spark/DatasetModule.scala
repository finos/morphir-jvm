package morphir.flowz.spark

import morphir.flowz._
import morphir.flowz.spark.sparkModule.SparkModule
import org.apache.spark.sql.{ Dataset, Encoder, SparkSession }
import zio.ZIO
import scala.reflect.runtime.universe.TypeTag
trait DatasetModule { self =>

  def filterDataset[State, DataRow, Exclude: Encoder: TypeTag, Include: Encoder: TypeTag](
    func: SparkSession => (State, DataRow) => (State, FilterResult[Exclude, Include])
  ): Behavior[State, State, Dataset[DataRow], SparkModule, Throwable, Dataset[FilterResult[Exclude, Include]]] =
    new Behavior[State, State, Dataset[DataRow], SparkModule, Throwable, Dataset[FilterResult[Exclude, Include]]] {
      protected def behavior(
        state: State,
        message: Dataset[DataRow]
      ): ZIO[SparkModule, Throwable, BehaviorResult[State, Dataset[FilterResult[Exclude, Include]]]] =
        ZIO.environment[SparkModule].mapEffect { sparkModule =>
          val spark       = sparkModule.get.sparkSession
          var outputState = state
          val inputData   = message
          import spark.implicits._
          val dataset = inputData.map { row =>
            val (nextState, filterRow) = func(spark)(outputState, row)
            outputState = nextState
            filterRow
          }
          BehaviorResult(state = outputState, value = dataset)
        }
    }

}

trait DatasetExports { exports => }
