package morphir.flowz.spark

import morphir.flowz._
import morphir.flowz.spark.sparkModule.SparkModule
import org.apache.spark.sql.{ Dataset, Encoder, SparkSession }
import zio.ZIO
import scala.reflect.runtime.universe.TypeTag
trait DatasetModule { self =>

  def filterDataset[State, DataRow, Exclude: Encoder: TypeTag, Include: Encoder: TypeTag](
    func: SparkSession => (State, DataRow) => (State, FilterResult[Exclude, Include])
  ): Act[State, State, SparkModule, Dataset[DataRow], Throwable, Dataset[FilterResult[Exclude, Include]]] =
    Act[State, State, SparkModule, Dataset[DataRow], Throwable, Dataset[FilterResult[Exclude, Include]]](
      ZIO.environment[StageContext[SparkModule, State, Dataset[DataRow]]].mapEffect { ctx =>
        val spark       = ctx.environment.get.sparkSession
        var outputState = ctx.inputs.state
        val inputData   = ctx.inputs.params

        import spark.implicits._
        val dataset = inputData.map { row =>
          val (nextState, filterRow) = func(spark)(outputState, row)
          outputState = nextState
          filterRow
        }
        StepOutputs(state = outputState, value = dataset)
      }
    )
}

trait DatasetExports { exports => }
