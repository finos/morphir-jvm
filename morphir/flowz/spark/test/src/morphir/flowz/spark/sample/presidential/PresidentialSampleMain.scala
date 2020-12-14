package morphir.flowz.spark.sample.presidential

import morphir.flowz.spark.{ default, sparkModule }
import default._
import sparkModule.SparkModule
import flowzApi._
import org.apache.spark.sql.{ DataFrame, SparkSession }
import zio._

object PresidentialSampleMain extends App {
  def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    val sparkBuilder = SparkSession
      .builder()
      .config("spark.debug.maxToStringFields", "200")
      .appName("Heroes Sample")
      .master("local")

    val customLayer = {
      SparkModule.buildLayer(sparkBuilder)
    }

    import flows._
    val flow = loadDataSources >>> summarizeData

    flow
      .run(
        models.Options(
          executiveFilePath = "morphir/flowz/spark/test/resources/data/in/executive.json",
          legislatorsPaths = List(
            "morphir/flowz/spark/test/resources/data/in/legislators-current.csv",
            "morphir/flowz/spark/test/resources/data/in/legislators-historical.csv"
          )
        )
      )
      .provideCustomLayer(customLayer)
      .exitCode
  }

  object flows {
    val loadExecutiveBranchInfo = sparkStep { spark => params: models.Options =>
      spark.read.option("multiLine", true).option("mode", "PERMISSIVE").json(params.executiveFilePath)
    }

    val loadLegislators = sparkStep { spark => params: models.Options =>
      spark.read.option("header", true).option("inferSchema", true).csv(params.legislatorsPaths: _*)
    }

    val loadDataSources = stage { (_: Any, options: models.Options) =>
      Flow.fromEffect(console.putStrLn(s"Options: $options")) *>
        (loadExecutiveBranchInfo |+| loadLegislators).map(models.RawDataSources.tupled)
    }

//    val Executive = {
//      import zio.prelude._
//      val df: DataFrame = ???
//      df.map(r => r.getAs[Int](""))
//    }

    //TODO: List all presidents who also served in congress

    val summarizeData = flowM { (state: Any, dataSources: models.RawDataSources) =>
      for {
        _ <- console.putStrLn(s"Executive Branch Data: $state====================================")
        _ <- ZIO.effect(dataSources.executiveData.printSchema())
        _ <- ZIO.effect(dataSources.executiveData.show(false))
        _ <- console.putStrLn(s"Legislature Data: $state=========================================")
        _ <- ZIO.effect(dataSources.currentLegislators.printSchema())
        _ <- ZIO.effect(dataSources.currentLegislators.show(false))
      } yield ((), ())
    }
  }

  object models {
    final case class Options(executiveFilePath: String, legislatorsPaths: Seq[String])
    final case class RawDataSources(executiveData: DataFrame, currentLegislators: DataFrame)
  }
}
