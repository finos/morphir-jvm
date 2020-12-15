package morphir.flowz.spark.sample.presidential

import morphir.flowz.spark.{ default, sparkModule }
import default._
import sparkModule.SparkModule
import flowzApi._
import org.apache.spark.sql.{ DataFrame, Dataset, SparkSession }
import org.apache.spark.sql.functions.{ explode, struct }
import zio._

object PresidentialSampleMain extends App {
  import models._
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
    val flow = initialize >>> prepareDataSources >>> createReport >>> printReport

    flow
      .run(
        Options(
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
    val loadExecutiveBranchInfo = sparkStep { spark => params: Options =>
      spark.read.option("multiLine", true).option("mode", "PERMISSIVE").json(params.executiveFilePath)
    }

    val loadLegislators = sparkStep { spark => params: Options =>
      spark.read.option("header", true).option("inferSchema", true).csv(params.legislatorsPaths: _*)
    }

    val initialize = stage { (_: Any, options: Options) =>
      Flow.fromEffect(console.putStrLn(s"Options: $options")) *>
        (loadExecutiveBranchInfo |+| loadLegislators).map { case (executiveData, legislatureData) =>
          RawDataSources(executiveData = executiveData, legislatureData = legislatureData)
        }.mapOutputs((_, out) => (out, out))
    }

    val collectExecutiveBranchPoliticians = stage { (dataSources: RawDataSources, _: Any) =>
      sparkStep { spark => _: Any =>
        import spark.implicits._
        val executiveDF = dataSources.executiveData
        executiveDF
          .select($"name", explode($"terms.type") as "position", $"bio.birthday" as "birthday")
          .distinct()
          .as[Executive]

      }.stateAs(dataSources)
    }

    val collectLegislativeBranchPoliticians = stage { (dataSources: RawDataSources, _: Any) =>
      sparkStep { spark => _: Any =>
        import spark.implicits._
        val executiveDF = dataSources.legislatureData
        val executiveDS = executiveDF
          .select(
            struct($"last_name" as "last", $"first_name" as "first", $"middle_name" as "middle") as "name",
            $"birthday",
            $"type" as "position"
          )
          .distinct()
          .as[CongressPerson]
        executiveDS
      }.stateAs(dataSources)

    }

    val prepareDataSources = stage { (rawData: RawDataSources, _: Any) =>
      collectExecutiveBranchPoliticians.zipWithPar(collectLegislativeBranchPoliticians) { (lOut, rOut) =>
        val dataSources = DataSources(raw = rawData, executive = lOut.value, congress = rOut.value)
        OutputChannels(state = dataSources, value = dataSources)
      }
    }

    val createReport = flowM { (data: DataSources, _: Any) =>
      for {
        numPrez     <- ZIO.effect(data.executive.filter(p => p.position == "prez").count())
        numVeep     <- ZIO.effect(data.executive.filter(p => p.position == "viceprez").count())
        numSenators <- ZIO.effect(data.congress.filter(p => p.position == "sen").count())
        numReps     <- ZIO.effect(data.congress.filter(p => p.position == "rep").count())
      } yield (
        data,
        Report(
          numberOfPresidents = numPrez,
          numberOfVicePresidents = numVeep,
          numberOfSenators = numSenators,
          numberOfRepresentatives = numReps
        )
      )
    }

    val printReport = flowM { (data: DataSources, report: Report) =>
      for {
        _ <- console.putStrLn(s"Number of Presidents: ${report.numberOfPresidents}")
        _ <- console.putStrLn(s"Number of Vice Presidents: ${report.numberOfVicePresidents}")
        _ <- console.putStrLn(s"Number of Senators: ${report.numberOfSenators}")
        _ <- console.putStrLn(s"Number of Representatives: ${report.numberOfRepresentatives}")
      } yield (data, report)
    }

  }

  object models {
    final case class Options(executiveFilePath: String, legislatorsPaths: Seq[String])
    final case class RawDataSources(executiveData: DataFrame, legislatureData: DataFrame)
    final case class DataSources(raw: RawDataSources, congress: Dataset[CongressPerson], executive: Dataset[Executive])

    final case class Report(
      numberOfPresidents: Long,
      numberOfVicePresidents: Long,
      numberOfSenators: Long,
      numberOfRepresentatives: Long
    )

    final case class Politician(name: Name, birthday: String, position: String)
    final case class CongressPerson(name: Name, birthday: String, position: String) {
      def toPolitician: Politician = Politician(name = name, birthday = birthday, position = position)
    }

    final case class Executive(name: Name, birthday: String, position: String) {
      def toPolitician: Politician = Politician(name = name, birthday = birthday, position = position)
    }
    final case class President(name: Name)
    final case class VicePresident(name: Name)

    final case class Term(`type`: String) {
      def termType: String = `type`
    }
    final case class Name(first: String, last: String, middle: Option[String])
  }
}
