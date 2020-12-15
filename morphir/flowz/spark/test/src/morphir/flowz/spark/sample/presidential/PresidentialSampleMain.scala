package morphir.flowz.spark.sample.presidential

import morphir.flowz.spark.{ default, sparkModule }
import default._
import sparkModule.SparkModule
import flowzApi._
import org.apache.spark.sql.{ DataFrame, Dataset, SparkSession }
import org.apache.spark.sql.functions.explode
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
    val flow = initialize >>> collectExecutiveBranchPoliticians >>> reportOnWorkflow

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
          models.Workflow(
            dataSources = models.RawDataSources(executiveData = executiveData, currentLegislators = legislatureData),
            stateName = "Initial",
            state = models.State.Initial
          )
        }.mapOutputs((_, out) => (out, out))
    }

    val collectExecutiveBranchPoliticians = stage { (workflow: Workflow[State], _: Any) =>
      sparkStep { spark => _: Any =>
        import spark.implicits._
        val executiveDF = workflow.dataSources.executiveData
        val executiveDS = executiveDF
          .select($"name", explode($"terms.type") as "position", $"bio.birthday" as "birthday")
          .distinct()
          .as[Politician]
        workflow.applyEvent(Event.RetrievedExecutiveBranchPoliticians(executiveDS))
      }.unifyOutputs
    }

    val reportOnWorkflow = stage { (state: Workflow[State], _: Any) =>
      Step.fromEffect {
        state match {
          case Workflow(_, stateName, State.Initial) => console.putStrLn(s"State is: $stateName")
          case Workflow(_, stateName, State.State01(politiciansFromExecutiveBranch)) =>
            for {
              _           <- console.putStrLn(s"State is: $stateName")
              politicians <- ZIO.effect(politiciansFromExecutiveBranch.collect().toList)
              eol         <- system.lineSeparator
              message <- ZIO.effectTotal(
                           politicians.zipWithIndex.mkString(
                             s"Executive Branch Politicians: =========================================== $eol",
                             eol,
                             "==========================================="
                           )
                         )
              _ <- console.putStrLn(message)
            } yield ()
          case Workflow(_, stateName, _) =>
            console.putStrLn(s"State is: $stateName")
        }
      }.stateAs(state)
    }

    //TODO: List all presidents who also served in congress

    val summarizeData = flowM { (state: Workflow[State.State01], _: Any) =>
      for {
        _ <-
          console.putStrLn(
            s"Executive Branch Data Count: ${state.dataSources.executiveData.count()}===================================="
          )
        _ <- ZIO.effect(state.dataSources.executiveData.printSchema())
        _ <- ZIO.effect(state.dataSources.executiveData.show(false))
        _ <-
          console.putStrLn(
            s"Legislature Data Count: ${state.dataSources.currentLegislators.count()}========================================="
          )
        _ <- ZIO.effect(state.dataSources.currentLegislators.printSchema())
        _ <- ZIO.effect(state.dataSources.currentLegislators.show(false))
        _ <-
          console.putStrLn(
            s"Executive Data Counts: ${state.state.politiciansFromExecutiveBranch.count()}========================================="
          )
        _ <- ZIO.effect(state.state.politiciansFromExecutiveBranch.printSchema())
        _ <- ZIO.effect(state.state.politiciansFromExecutiveBranch.show(false))
      } yield ((), ())
    }
  }

  object models {
    final case class Options(executiveFilePath: String, legislatorsPaths: Seq[String])
    final case class RawDataSources(executiveData: DataFrame, currentLegislators: DataFrame)

    sealed trait Event
    object Event {
      final case class RetrievedExecutiveBranchPoliticians(politician: Dataset[Politician]) extends Event
    }

    final case class Workflow[+A <: State](dataSources: RawDataSources, stateName: String, state: A) { self =>
      def applyEvent(event: Event) = event match {
        case Event.RetrievedExecutiveBranchPoliticians(politicians) =>
          self.copy(stateName = "RetrievedExecutiveBranchPoliticians", state = State.State01(politicians))
      }
    }

    sealed trait State extends Product with Serializable
    object State {
      case object Initial extends State {
        def apply(politicians: Dataset[Politician]): State01 =
          State01(politicians)
      }

      final case class State01(
        politiciansFromExecutiveBranch: Dataset[Politician]
      ) extends State

      final case class State02(
        vicePresidents: Dataset[Executive],
        presidents: Dataset[Executive]
      ) extends State
    }

    final case class Politician(name: Name, birthday: String, position: String)

    final case class Executive(name: Name, term: Term)
    final case class President(name: Name)
    final case class VicePresident(name: Name)

    final case class Term(`type`: String) {
      def termType: String = `type`
    }
    final case class Name(first: String, last: String, middle: Option[String])
  }
}
