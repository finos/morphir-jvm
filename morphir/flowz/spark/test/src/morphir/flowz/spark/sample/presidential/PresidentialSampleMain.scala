package morphir.flowz.spark.sample.presidential

import morphir.flowz.api._
import morphir.flowz.spark.api._
import sparkModule.SparkModule
import org.apache.spark.sql.{ DataFrame, Dataset, SQLContext, SparkSession }
import org.apache.spark.sql.functions.{ explode, struct }
import zio._

import scala.annotation.nowarn

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
      Step.fromEffect(console.putStrLn(s"Options: $options")) *>
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
        val dataFrame = dataSources.legislatureData
        val executiveDS = dataFrame
          .select(
            struct($"last_name" as "last", $"first_name" as "first", $"middle_name" as "middle") as "name",
            $"birthday",
            $"type" as "position",
            $"state"
          )
          .distinct()
          .as[CongressPerson]
        executiveDS
      }.stateAs(dataSources)

    }

    val getPresidentsCongressionalServiceRecord = stage { (data: DataSources, _: Any) =>
      sparkStep { spark => _: Any =>
        import spark.implicits._
        @nowarn
        implicit val sqlContext: SQLContext = spark.sqlContext
        import io.getquill.QuillSparkContext
        import io.getquill.QuillSparkContext._

        val presidents = data.executive.filter(row => row.position == "prez")
        val congress   = data.congress
        QuillSparkContext.run {
          for {
            president <- liftQuery(presidents)
            congressPerson <-
              liftQuery(congress).leftJoin(congressPerson =>
                president.name.first == congressPerson.name.first && president.name.last == congressPerson.name.last
              )
          } yield President(
            name = president.name,
            birthday = president.birthday,
            servedInCongress = congressPerson.map(_.state).isDefined
          )
        }.distinct()
      }.stateAs(data)

    }

    val prepareDataSources = stage { (rawData: RawDataSources, _: Any) =>
      Step.mapParN(
        collectExecutiveBranchPoliticians,
        collectLegislativeBranchPoliticians
      ) { (executive, congressional) =>
        val dataSources = DataSources(
          raw = rawData,
          executive = executive.value,
          congress = congressional.value
        )
        StepOutputs(state = dataSources, value = dataSources)
      } >>> getPresidentsCongressionalServiceRecord.mapOutputs { (dataSources, presidents) =>
        val finalData = FinalDataSources(
          raw = dataSources.raw,
          congress = dataSources.congress,
          executive = dataSources.executive,
          presidents = presidents
        )
        (finalData, finalData)
      }
    }

    val createReport = Step { (data: FinalDataSources, _: Any) =>
      for {
        numPrez     <- ZIO.effect(data.executive.filter(p => p.position == "prez").count())
        numVeep     <- ZIO.effect(data.executive.filter(p => p.position == "viceprez").count())
        numSenators <- ZIO.effect(data.congress.filter(p => p.position == "sen").count())
        numReps     <- ZIO.effect(data.congress.filter(p => p.position == "rep").count())
        presidentsWhoServedInCongress <-
          ZIO.effect(data.presidents.filter(pres => pres.servedInCongress).distinct().collect().toList)
        numPrezWithCongressionalRecs <- ZIO.succeed(presidentsWhoServedInCongress.size)
      } yield StepOutputs(
        state = data,
        value = Report(
          numberOfPresidents = numPrez,
          numberOfVicePresidents = numVeep,
          numberOfSenators = numSenators,
          numberOfRepresentatives = numReps,
          numberOfPresidentsWhoServedInCongress = numPrezWithCongressionalRecs,
          presidentsWhoServedInCongress = presidentsWhoServedInCongress
        )
      )
    }

    val printReport = Step { (data: FinalDataSources, report: Report) =>
      for {
        _ <- console.putStrLn(s"Number of Presidents: ${report.numberOfPresidents}")
        _ <- console.putStrLn(s"Number of Vice Presidents: ${report.numberOfVicePresidents}")
        _ <- console.putStrLn(s"Number of Senators: ${report.numberOfSenators}")
        _ <- console.putStrLn(s"Number of Representatives: ${report.numberOfRepresentatives}")
        _ <- console.putStrLn(
               s"Number of Presidents who served in congress: ${report.numberOfPresidentsWhoServedInCongress}"
             )
        _   <- console.putStrLn("====")
        eol <- system.lineSeparator
        presidentialReport <- ZIO.effect {
                                report.presidentsWhoServedInCongress
                                  .sortBy(_.birthday)
                                  .mkString(
                                    s"Presidents Who Served In Congress =============================$eol",
                                    eol,
                                    "===================================="
                                  )
                              }
        _ <- console.putStrLn(presidentialReport)
      } yield StepOutputs(state = data, value = report)
    }

  }

  object models {
    final case class Options(executiveFilePath: String, legislatorsPaths: Seq[String])
    final case class RawDataSources(executiveData: DataFrame, legislatureData: DataFrame)
    final case class DataSources(
      raw: RawDataSources,
      congress: Dataset[CongressPerson],
      executive: Dataset[Executive]
    )
    final case class FinalDataSources(
      raw: RawDataSources,
      congress: Dataset[CongressPerson],
      executive: Dataset[Executive],
      presidents: Dataset[President]
    )

    final case class Report(
      numberOfPresidents: Long,
      numberOfVicePresidents: Long,
      numberOfSenators: Long,
      numberOfRepresentatives: Long,
      numberOfPresidentsWhoServedInCongress: Int,
      presidentsWhoServedInCongress: List[President]
    )

    final case class Politician(name: Name, birthday: String, position: String)
    final case class CongressPerson(name: Name, birthday: String, position: String, state: String) {
      def toPolitician: Politician = Politician(name = name, birthday = birthday, position = position)
    }

    final case class Executive(name: Name, birthday: String, position: String) {
      def toPolitician: Politician = Politician(name = name, birthday = birthday, position = position)
    }
    final case class President(name: Name, birthday: String, servedInCongress: Boolean)
    final case class VicePresident(name: Name)

    final case class Term(`type`: String) {
      def termType: String = `type`
    }
    final case class Name(first: String, last: String, middle: Option[String])
  }
}
