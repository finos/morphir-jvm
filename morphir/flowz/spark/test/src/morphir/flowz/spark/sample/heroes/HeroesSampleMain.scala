package morphir.flowz.spark.sample.heroes

import morphir.flowz._
import morphir.flowz.spark.sparkModule.SparkModule
import morphir.flowz.spark.{ SparkFlow, SparkStep, SparkTaskStep }
import org.apache.spark.sql.{ Dataset, Encoder, SparkSession }
import zio._

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

  val loadAbilities: SparkStep[Any, Any, Throwable, Dataset[Ability]] = SparkFlow.createDataset {
    spark => implicit encoder: Encoder[Ability] =>
      spark.createDataset(abilities)
  }

  val loadHeroAbilities: SparkStep[Any, Any, Throwable, Dataset[HeroAbility]] = SparkFlow.createDataset {
    spark => implicit encoder: Encoder[HeroAbility] =>
      spark.createDataset(heroAbilities)
  }

  val loadPeople: SparkTaskStep[Any, Dataset[Person]] = SparkFlow.createDataset(people)

  val loadAlterEgos: SparkTaskStep[Any, Dataset[AlterEgo]] = SparkFlow.createDataset(alterEgos)

  val loadDataSources: Flow[Any, DataSources, Any with SparkModule, Any, Throwable, DataSources] =
    (for {
      abilities     <- loadAbilities >>> SparkFlow.showDataset(false)
      heroAbilities <- loadHeroAbilities >>> SparkFlow.showDataset(false)
      people        <- loadPeople >>> SparkFlow.showDataset(false)
      alterEgos     <- loadAlterEgos >>> SparkFlow.showDataset(false)
    } yield DataSources(
      abilities = abilities,
      heroAbilities = heroAbilities,
      people = people,
      alterEgos = alterEgos
    )).unifyOutputs

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

    val flow = loadDataSources
    flow
      .run(args)
      .provideCustomLayer(customLayer)
      .foldM(
        failure = err => console.putStrLn(s"Error encountered while running flow: $err"),
        success = output => console.putStrLn(s"Processed flow for ${output.value.people.count()} people")
      )
      .exitCode
  }

  final case class DataSources(
    abilities: Dataset[Ability],
    heroAbilities: Dataset[HeroAbility],
    people: Dataset[Person],
    alterEgos: Dataset[AlterEgo]
  )

}

final case class Person(firstName: String, lastName: String)
final case class AlterEgo(firstName: String, lastName: String, heroName: String, isSecret: Boolean)
final case class Hero(name: String, abilities: Set[Ability], alter: Option[AlterEgo])
final case class HeroAbility(hero: String, ability: String)
final case class Ability(name: String, description: String)
