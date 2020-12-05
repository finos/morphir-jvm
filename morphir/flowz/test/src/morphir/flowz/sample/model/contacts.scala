package morphir.flowz.sample.model

import zio.prelude._
import zio.random.Random
import zio.test.Gen

import java.time.LocalDate

object contacts {

  object BirthDate extends Newtype[LocalDate] {
    object generators {
      val default: Gen[Random, contacts.BirthDate.newtype.Type with contacts.BirthDate.Tag] =
        Gen
          .localDateTime(LocalDate.now().minusYears(120).atStartOfDay(), LocalDate.now().atStartOfDay().plusHours(23))
          .map(date => BirthDate(date.toLocalDate))
    }
  }

  type BirthDate = BirthDate.Type

  object FirstName extends Subtype[String] {
    object generators {
      val default: Gen[Any, FirstName] =
        Gen.fromIterable(Seq("Adam", "John", "Stan", "Koby", "Mike").map(FirstName(_)))
    }
  }
  type FirstName = FirstName.Type

  object LastName extends Subtype[String] {
    object generators {
      val default: Gen[Any, LastName] =
        Gen.fromIterable(Seq("Tyson", "Bowe", "Smith", "Lewis", "Bryant", "Jordan").map(LastName(_)))
    }
  }
  type LastName = LastName.Type

  object PhoneNumber extends Subtype[String]
  type PhoneNumber = PhoneNumber.Type

  object PhoneNumberLabel extends Subtype[String]
  type PhoneNumberLabel = PhoneNumberLabel.Type

  final case class PersonRow(
    firstName: String,
    lastName: String,
    dateOfBirth: String,
    phoneNumber: String,
    phoneNumberKind: String
  )
  object PersonRow {
    object generators {}
  }
  final case class Person(firstName: FirstName, lastName: LastName, birthDate: BirthDate)
  final case class Contact(firstName: FirstName, lastName: LastName, phoneNumbers: Map[PhoneNumberLabel, PhoneNumber])

}
