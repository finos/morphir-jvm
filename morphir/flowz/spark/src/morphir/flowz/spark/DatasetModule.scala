package morphir.flowz.spark

import morphir.flowz.FilterResult
import morphir.flowz.spark.sparkModule.SparkModule
import org.apache.spark.sql.{ Dataset, Encoder, SparkSession }
import zio.ZIO
import scala.reflect.runtime.universe.TypeTag
trait DatasetModule { self: FlowzSparkModule =>
  import flowzApi._

  def filterDataset[State, DataRow, Exclude: Encoder: TypeTag, Include: Encoder: TypeTag](
    func: SparkSession => (State, DataRow) => (State, FilterResult[Exclude, Include])
  ): Flow[State, State, SparkModule, Dataset[DataRow], Throwable, Dataset[FilterResult[Exclude, Include]]] =
    Flow[State, State, SparkModule, Dataset[DataRow], Throwable, Dataset[FilterResult[Exclude, Include]]](
      ZIO.environment[FlowContext[SparkModule, State, Dataset[DataRow]]].mapEffect { ctx =>
        val spark       = ctx.environment.get.sparkSession
        var outputState = ctx.inputs.state
        val inputData   = ctx.inputs.params

        import spark.implicits._
        val dataset = inputData.map { row =>
          val (nextState, filterRow) = func(spark)(outputState, row)
          outputState = nextState
          filterRow
        }
        FOuts(state = outputState, value = dataset)
      }
    )
}

trait DatasetExports { exports =>
  val flowzApi: morphir.flowz.Api
  //import flowzApi._

  trait DatasetCatalog extends DatasetModule { self: FlowzSparkModule => }
}
