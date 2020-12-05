package morphir.flowz.spark.sample.model

import zio.random.Random
import zio.test._

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
