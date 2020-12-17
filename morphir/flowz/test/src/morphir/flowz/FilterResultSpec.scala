package morphir.flowz

import morphir.flowz.FilterResult.{ Excluded, Included }
import zio.test.Assertion._
import zio.test._

import scala.annotation.nowarn

object FilterResultSpec extends DefaultRunnableSpec {
  def hasIncludedField[X, I](assertion: Assertion[Option[I]]): Assertion[FilterResult[X, I]] =
    hasField[FilterResult[X, I], Option[I]]("included", _.included, assertion)

  def hasExcludedField[X, I](assertion: Assertion[Option[X]]): Assertion[FilterResult[X, I]] =
    hasField[FilterResult[X, I], Option[X]]("excluded", _.excluded, assertion)

  def spec = suite("FilterResult Spec")(
    test("It should support creating an Included filter result") {
      val actual = FilterResult.included(42)
      assert(actual)(equalTo(Included(42)) && hasIncludedField(isSome(equalTo(42))) && hasExcludedField(isNone))
    },
    test("It should support creating an Excluded filter result") {
      val actual = FilterResult.excluded("NOPE!")
      assert(actual)(
        equalTo(Excluded("NOPE!")) && hasExcludedField(isSome(equalTo("NOPE!"))) && hasIncludedField(isNone)
      )
    },
    test("It should support a value method that unifies the result types.") {
      val sut: FilterResult[Thing1, Thing2] = FilterResult.included(Thing2("Two"))
      val actual                            = sut.value
      assert(actual)(equalTo(Thing2("Two")))
    },
    test("It should support a toEither method that converts Excluded results to Lefts") {
      val actual: Either[String, BigDecimal] = FilterResult.excluded("Rejected!!!").includedAs[BigDecimal].toEither
      assert(actual)(isLeft(equalTo("Rejected!!!")))
    },
    test("It should support a toEither method that converts Included results to Rights") {
      val actual: Either[String, Boolean] = FilterResult.included(true).excludedAs[String].toEither
      assert(actual)(isRight(equalTo(true)))
    },
    test("It should support a toOption method that converts Included results to a Some()") {
      val value                  = Thing1("Alpha")
      val actual: Option[Thing1] = FilterResult.included(value).toOption
      assert(actual)(isSome(equalTo(value)))
    },
    test("It should support a toOption method that converts Excluded results to a None") {
      val value                  = Thing1("Alpha")
      val actual: Option[Thing1] = FilterResult.excluded(value).toOption
      assert(actual)(isNone)
    },
    test("It should support folding an Included result") {
      val sut = FilterResult.included("A")
      @nowarn
      val actual = sut.fold(v => s"Excluded: $v")(v => s"Included: $v")
      assert(actual)(equalTo("Included: A"))
    },
    test("It should support folding an Excluded result") {
      val sut = FilterResult.excluded("A")
      @nowarn
      val actual = sut.fold(v => s"Excluded: $v")(v => s"Included: $v")
      assert(actual)(equalTo("Excluded: A"))
    },
    test("It should support pattern matching on an Included value") {
      val elements: List[FilterResult[String, Int]] =
        List(FilterResult.included(1), FilterResult.included(2), FilterResult.excluded("Three"))
      val actual = elements.collect {
        case FilterResult.Included(1) => "One"
        case FilterResult.Included(2) => "Two"
        case FilterResult.Included(n) => n.toString
      }
      assert(actual)(equalTo(List("One", "Two")))
    },
    test("It should support pattern matching on an Excluded value") {
      val elements: List[FilterResult[Int, String]] =
        List(FilterResult.excluded(1), FilterResult.excluded(2), FilterResult.included("Three"))

      val actual = elements.collect {
        case FilterResult.Excluded(1) => "One"
        case FilterResult.Excluded(2) => "Two"
      }
      assert(actual)(equalTo(List("One", "Two")))
    },
    test("It should support construction from an Option") {
      val initial = List(Option("Hello"), None, Option("Goodbye"))
      val actual  = initial.map(FilterResult.fromOption)
      assert(actual)(
        equalTo(List(FilterResult.included("Hello"), FilterResult.excluded(None), FilterResult.included("Goodbye")))
      )
    },
    test("It should support construction from an Either") {
      val initial = List(Left("A"), Right(1), Left("B"), Right(2))
      val actual  = initial.map(FilterResult.fromEither)
      assert(actual)(
        equalTo(
          List(
            FilterResult.excluded("A"),
            FilterResult.included(1),
            FilterResult.excluded("B"),
            FilterResult.included(2)
          )
        )
      )
    }
  )

  trait Thing {
    def name: String
  }
  final case class Thing1(name: String) extends Thing
  final case class Thing2(name: String) extends Thing

}
