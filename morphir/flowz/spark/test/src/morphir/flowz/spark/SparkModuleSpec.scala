package morphir.flowz.spark

import morphir.flowz.spark.sparkModule.SparkModule
import morphir.flowz.spark.testing.SparkSpec
import zio.ZIO
import zio.random.Random
import zio.test._
import zio.test.Assertion._

object SparkModuleSpec extends SparkSpec {
  def spec = suite("SparkModule Spec")(
    suite("Layer Creation")(
      testM("It should be possible to create a layer from a builder")(
        for {
          actual <- ZIO.environment[SparkModule].run
        } yield assert(actual)(succeeds(anything))
      )
    ),
    suite("Calling createDataset")(
      testM("It should support Dataset creation of a case class")(
        checkM(Person.genPeople(10, 30)) { people =>
          for {
            actualDataset <- sparkModule.createDataset(people.toList)
            actual        <- ZIO.effect(actualDataset.collect().toSet)
          } yield assert(actual)(equalTo(people))
        }
      )
    ),
    suite("Calling printSchema")(
      testM("It should support printing the schema of a DataFrame")(
        for {
          dataFrame <- sparkModule.withSpark(_.createDataFrame[Person](Seq.empty))
          result    <- sparkModule.printSchema(dataFrame).run
        } yield assert(result)(succeeds(anything))
      )
    ),
    suite("Calling makeDataset")(
      testM("It should support creation of a Dataset given just a sparkSession")(
        for {
          data   <- ZIO.succeed(
                      Seq(
                        ("James ", "", "Smith", "36636", "M", 3000),
                        ("Michael ", "Rose", "", "40288", "M", 4000),
                        ("Robert ", "", "Williams", "42114", "M", 4000),
                        ("Maria ", "Anne", "Jones", "39192", "F", 4000),
                        ("Jen", "Mary", "Brown", "", "F", -1)
                      )
                    )
          columns = Seq("firstname", "middlename", "lastname", "dob", "gender", "salary")
          actual <- sparkModule.makeDataset { spark =>
                      import spark.sqlContext.implicits._
                      val df = data.toDF(columns: _*)
                      val ds = df.as[(String, String, String, String, String, Int)]
                      ds.show(true)
                      ds
                    }
          actual <- ZIO.effect(actual.collect().toList)
        } yield assert(actual)(equalTo(data.toList))
      )
    )
  )

  final case class FirstName(value: String)

  object FirstName {
    val gen: Gen[Any, FirstName] =
      Gen
        .fromIterable(Seq("Albert", "Bobby", "Carl", "Claude", "David", "George", "Thomas", "William", "Xavier"))
        .map(FirstName(_))
  }

  final case class LastName(value: String)
  object LastName {
    val gen: Gen[Any, LastName] =
      Gen
        .fromIterable(
          Seq("Alexander", "Black", "Brown", "James", "Jefferson", "Jones", "Smith", "Washington", "Williams")
        )
        .map(LastName(_))
  }

  final case class Person(firstName: FirstName, lastName: LastName)
  object Person {
    val gen: Gen[Any, Person] =
      for {
        firstName <- FirstName.gen
        lastName  <- LastName.gen
      } yield Person(firstName, lastName)

    def genPeople(min: Int, max: Int): Gen[Random with Sized, Set[Person]] =
      Gen.setOfBounded(min, max)(gen)
  }
}
