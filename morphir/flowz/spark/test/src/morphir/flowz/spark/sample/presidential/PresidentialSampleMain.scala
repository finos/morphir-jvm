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
        executiveDF
          .select($"name", explode($"terms.type") as "position", $"bio.birthday" as "birthday")
          .distinct()
          .as[Politician]

      }.mapState(ds => workflow.applyEvent(Event.RetrievedExecutiveBranchPoliticians(ds)))
    }

    val collectLegislativeBranchPoliticians = stage { (workflow: Workflow[State], _: Any) =>
      sparkStep { spark => _: Any =>
        import spark.implicits._
        val executiveDF = workflow.dataSources.executiveData
        val executiveDS = executiveDF
          .select(
            struct($"last_name" as "last", $"first_name" as "first", $"middle_name" as "middle") as "name",
            $"birthday"
          )
          .distinct()
          .as[Politician]
        executiveDS
      }.mapState(ds => workflow.applyEvent(Event.RetrievedExecutiveBranchPoliticians(ds)))

    }

//    val collectPoliticians =
//      collectExecutiveBranchPoliticians.zipWithPar(collectLegislativeBranchPoliticians) { (lOut, rOut) => }

    val reportOnWorkflow = stage { (state: Workflow[State], _: Any) =>
      Step.fromEffect {
        state match {
          case Workflow(_, stateName, State.Initial) => console.putStrLn(s"State is: $stateName")
          case Workflow(_, stateName, State.WithExecutive(politiciansFromExecutiveBranch)) =>
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

    val summarizeData = flowM { (state: Workflow[State.WithExecutive], _: Any) =>
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
      final case class RetrievedExecutiveBranchPoliticians(politician: Dataset[Politician])   extends Event
      final case class RetrievedLegislativeBranchPoliticians(politician: Dataset[Politician]) extends Event
    }

    final case class Workflow[+A <: State](dataSources: RawDataSources, stateName: String, state: A) { self =>
      def applyEvent(event: Event) = (state, event) match {
        case (State.WithExecutive(_), Event.RetrievedExecutiveBranchPoliticians(politicians)) =>
          self.copy(stateName = "WithExecutive", state = State.WithExecutive(politicians))
        case (State.WithLegislative(legislative), Event.RetrievedExecutiveBranchPoliticians(executive)) =>
          self.copy(
            stateName = "WithPoliticians",
            state = State.WithPoliticians(
              politiciansFromLegislature = legislative,
              politiciansFromExecutiveBranch = executive
            )
          )
        case (State.WithPoliticians(_, legislative), Event.RetrievedExecutiveBranchPoliticians(executive)) =>
          self.copy(
            stateName = "WithPoliticians",
            state = State.WithPoliticians(
              politiciansFromLegislature = legislative,
              politiciansFromExecutiveBranch = executive
            )
          )
        case (_, Event.RetrievedExecutiveBranchPoliticians(politicians)) =>
          self.copy(stateName = "WithExecutive", state = State.WithExecutive(politicians))
        case (State.WithLegislative(_), Event.RetrievedLegislativeBranchPoliticians(politicians)) =>
          self.copy(
            stateName = "WithLegislative",
            state = State.WithLegislative(
              politiciansFromLegislature = politicians
            )
          )
        case (State.WithExecutive(executive), Event.RetrievedLegislativeBranchPoliticians(politicians)) =>
          self.copy(
            stateName = "WithPoliticians",
            state = State.WithPoliticians(
              politiciansFromExecutiveBranch = executive,
              politiciansFromLegislature = politicians
            )
          )
        case (State.WithPoliticians(executive, _), Event.RetrievedLegislativeBranchPoliticians(politicians)) =>
          self.copy(
            stateName = "WithPoliticians",
            state = State.WithPoliticians(
              politiciansFromExecutiveBranch = executive,
              politiciansFromLegislature = politicians
            )
          )
        case (_, Event.RetrievedLegislativeBranchPoliticians(politicians)) =>
          self.copy(
            stateName = "WithLegislative",
            state = State.WithLegislative(
              politiciansFromLegislature = politicians
            )
          )
      }
    }

    sealed trait State extends Product with Serializable
    object State {
      case object Initial extends State {
        def apply(politicians: Dataset[Politician]): WithExecutive =
          WithExecutive(politicians)
      }

      final case class WithExecutive(
        politiciansFromExecutiveBranch: Dataset[Politician]
      ) extends State

      final case class WithLegislative(
        politiciansFromLegislature: Dataset[Politician]
      ) extends State

      final case class WithPoliticians(
        politiciansFromExecutiveBranch: Dataset[Politician],
        politiciansFromLegislature: Dataset[Politician]
      ) extends State
    }

    final case class Politician(name: Name, birthday: String, position: String)
    final case class CongressPerson(name: Name, birthday: String, position: String)

    final case class Executive(name: Name, term: Term)
    final case class President(name: Name)
    final case class VicePresident(name: Name)

    final case class Term(`type`: String) {
      def termType: String = `type`
    }
    final case class Name(first: String, last: String, middle: Option[String])
  }
}
