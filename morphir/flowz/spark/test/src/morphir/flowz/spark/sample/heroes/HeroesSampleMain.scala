package morphir.flowz.spark.sample.heroes

import io.getquill.QuillSparkContext
import morphir.flowz._
import morphir.flowz.spark.sparkModule.SparkModule
import morphir.flowz.spark.{ SparkFlow, SparkStep, sparkModule }
import org.apache.spark.sql.{ Dataset, SQLContext, SparkSession }
import zio._
import zio.clock.Clock
import zio.console.Console
import zio.random.Random

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

object HeroesSampleMain extends App {

  val abilities = List(
    Ability(name = "flight", description = "Defy gravity by taking to the sky"),
    Ability(name = "super strength", description = "Strength beyond the strength of a normal man or woman."),
    Ability(name = "genius intellect", description = "Smarter than the average bear"),
    Ability(name = "tactician", description = "Art of war and such."),
    Ability(name = "martial arts master", description = "Heeyah!!!"),
    Ability(name = "telepathy", description = "Able to read minds."),
    Ability(name = "telekinesis", description = "Able to move things with the mind.")
  )

  val heroAbilities = List(
    HeroAbility(hero = "Superman", ability = "flight"),
    HeroAbility(hero = "Superman", ability = "super strength"),
    HeroAbility(hero = "Batman", ability = "martial arts master"),
    HeroAbility(hero = "Batman", ability = "tactician"),
    HeroAbility(hero = "Ironman", ability = "tactician"),
    HeroAbility(hero = "Ironman", ability = "genius intellect"),
    HeroAbility(hero = "Ironman", ability = "flight"),
    HeroAbility(hero = "Ironman", ability = "super strength"),
    HeroAbility(hero = "Phoenix", ability = "telekinesis"),
    HeroAbility(hero = "Phoenix", ability = "telepathy"),
    HeroAbility(hero = "Phoenix", ability = "flight")
  )

  val people = List(
    Person(firstName = "Jessica", lastName = "Jones"),
    Person(firstName = "Jean", lastName = "Grey"),
    Person(firstName = "Clark", lastName = "Kent"),
    Person(firstName = "Bruce", lastName = "Banner"),
    Person(firstName = "Bruce", lastName = "Wayne"),
    Person(firstName = "Tony", lastName = "Stark")
  )

  val alterEgos = List(
    AlterEgo(firstName = "Bruce", lastName = "Banner", heroName = "The Incredible Hulk", isSecret = true),
    AlterEgo(firstName = "Clark", lastName = "Kent", heroName = "Superman", isSecret = true),
    AlterEgo(firstName = "Bruce", lastName = "Wayne", heroName = "Batman", isSecret = true),
    AlterEgo(firstName = "Tony", lastName = "Stark", heroName = "Ironman", isSecret = false),
    AlterEgo(firstName = "Jean", lastName = "Grey", heroName = "Phoenix", isSecret = true)
  )

  val loadAbilities: SparkStep[Clock with Console with Random, Options, Throwable, Dataset[Ability]] =
    createDataset(abilities)

  val loadHeroAbilities: SparkStep[Clock with Console with Random, Options, Throwable, Dataset[HeroAbility]] =
    createDataset(heroAbilities)

  val loadPeople: SparkStep[Clock with Console with Random, Options, Throwable, Dataset[Person]] =
    createDataset(people)

  val loadAlterEgos: SparkStep[Clock with Console with Random, Options, Throwable, Dataset[AlterEgo]] =
    createDataset(alterEgos)

  val loadDataSourcesPar: SparkStep[Clock with Console with Random, Options, Throwable, DataSources] =
    SparkFlow.mapParN(loadAbilities, loadHeroAbilities, loadPeople, loadAlterEgos) {
      case (abilitiesOut, heroAbilitiesOut, peopleOut, alterEgosOut) =>
        OutputChannels {
          DataSources(
            abilities = abilitiesOut.value,
            heroAbilities = heroAbilitiesOut.value,
            people = peopleOut.value,
            alterEgos = alterEgosOut.value
          )
        }
    }

  val loadDataSourcesSeq: SparkStep[Clock with Console with Random, Options, Throwable, DataSources] =
    (for {
      abilities     <- loadAbilities
      heroAbilities <- loadHeroAbilities
      people        <- loadPeople
      alterEgos     <- loadAlterEgos
    } yield DataSources(
      abilities = abilities,
      heroAbilities = heroAbilities,
      people = people,
      alterEgos = alterEgos
    ))

  val loadDataSources: SparkStep[Clock with Console with Random, Options, Throwable, DataSources] =
    SparkStep.parameters[Options].flatMap { options: Options =>
      if (options.parallelLoads)
        loadDataSourcesPar
      else
        loadDataSourcesSeq
    }

  val getAllHeroNames: SparkFlow[DataSources, DataSources, Any, Any, Throwable, Dataset[String]] =
    SparkStep
      .state[DataSources]
      .flatMap { dataSources =>
        SparkStep.withSpark { spark =>
          implicit val sqlContext: SQLContext = spark.sqlContext
          import sqlContext.implicits._
          import io.getquill.QuillSparkContext._

          QuillSparkContext.run {
            liftQuery(dataSources.heroAbilities).map(abilities => abilities.hero) union liftQuery(dataSources.alterEgos)
              .map(alter => alter.heroName)
          }
        }.stateAs(dataSources)
      }
      .tapValue(dataset => ZIO.effect(dataset.show(false)))

  val getAllHeroAbilities
    : SparkFlow[DataSources, DataSources, Any, Dataset[String], Throwable, Dataset[HeroAbilities]] =
    Step.context[Any, DataSources, Dataset[String]].flatMap { ctx =>
      val heroAbilities = ctx.inputs.state.heroAbilities
      val abilities     = ctx.inputs.state.abilities
      val heroNames     = ctx.inputs.params

      SparkStep.withSpark { spark =>
        implicit val sqlContext: SQLContext = spark.sqlContext
        import sqlContext.implicits._
        import io.getquill.QuillSparkContext._

        // Gather all the hero to ability details we have
        val heroAbilityDetails = QuillSparkContext.run {
          for {
            heroAbility <- liftQuery(heroAbilities)
            ability     <- liftQuery(abilities).join(ability => ability.name == heroAbility.ability)
          } yield HeroAbilityRow(hero = heroAbility.hero, ability = ability)
        }

        // Group by the hero name
        heroAbilityDetails.groupByKey(ha => ha.hero).mapGroups { (hero, row) =>
          HeroAbilities(hero = hero, abilities = row.map(_.ability).toSet)
        }

      }.stateAs(ctx.inputs.state)
    }

  val parseCommandLine: Step[Any, List[String], Nothing, Options] =
    Step.makeStep { args: List[String] =>
      ZIO.succeed {
        val useParallelSources = args match {
          case head :: _ if head.equalsIgnoreCase("parallel")   => true
          case head :: _ if head.equalsIgnoreCase("--parallel") => true
          case _                                                => false
        }
        Options(parallelLoads = useParallelSources)
      }
    }

  def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {

    val sparkBuilder = SparkSession
      .builder()
      .config("spark.debug.maxToStringFields", "200")
      .appName("Heroes Sample")
      .master("local")
    //.enableHiveSupport()

    val customLayer = {
      SparkModule.buildLayer(sparkBuilder)
    }

    val flow =
      parseCommandLine >>> loadDataSources.unifyOutputs >>> getAllHeroNames >>> getAllHeroAbilities >>> SparkStep
        .showDataset(false)
    flow
      .run(args)
      .provideCustomLayer(customLayer)
      .foldM(
        failure = err => console.putStrLn(s"Error encountered while running flow: $err"),
        success = output => console.putStrLn(s"The heroes we know are: ${output.value.collect().toList}")
      )
      .exitCode
  }

  def createDataset[A <: Product: ClassTag: TypeTag: zio.Tag](
    data: => Seq[A]
  ): SparkStep[Clock with Console with Random, Options, Throwable, Dataset[A]] =
    SparkStep.makeStep { options: Options =>
      val tag = zio.Tag[A]
      for {
        _     <- console.putStrLn(s"Creating/loading Dataset of type ${tag.tag.longName}")
        delay <- random.nextLongBetween(0, options.maxDelayInMillis).map(zio.duration.Duration.fromMillis)
        data  <- sparkModule.createDataset(data).delay(delay)
        _     <- console.putStrLn(s"Created/loaded Dataset of type ${tag.tag.longName}")
        _     <- sparkModule.withSpark(_ => data.show(false))
      } yield data
    }

  final case class DataSources(
    abilities: Dataset[Ability],
    heroAbilities: Dataset[HeroAbility],
    people: Dataset[Person],
    alterEgos: Dataset[AlterEgo]
  )

  final case class Options(parallelLoads: Boolean = false, showDatasets: Boolean = true, maxDelayInMillis: Long = 3000)
}

final case class Person(firstName: String, lastName: String)
final case class AlterEgo(firstName: String, lastName: String, heroName: String, isSecret: Boolean)
final case class Hero(name: String, abilities: Set[Ability], alter: Option[AlterEgo])
final case class HeroAbility(hero: String, ability: String)
final case class Ability(name: String, description: String)
final case class HeroAbilityRow(hero: String, ability: Ability)
final case class HeroAbilities(hero: String, abilities: Set[Ability])
