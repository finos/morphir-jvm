package zio.morphir.sexpr

import zio.morphir.sexpr.Gens._
import zio.morphir.sexpr._
import zio.morphir.testing.ZioBaseSpec
import zio.test._
import zio.test.Assertion._
import zio.test.TestAspect._

import java.time._

object RoundTripSpec extends ZioBaseSpec {
  def spec = suite("RoundTrip")(
    suite("primitives")(
      test("bigInt") {
        check(genBigInteger)(assertRoundtrips)
      } @@ samples(1000),
      test("bigDecimal") {
        check(genBigDecimal)(assertRoundtrips)
      } @@ samples(1000),
      test("boolean") {
        check(Gen.boolean)(assertRoundtrips)
      } @@ samples(1000),
      test("byte") {
        check(Gen.byte)(assertRoundtrips)
      } @@ samples(1000),
      test("char") {
        check(Gen.char)(assertRoundtrips)
      } @@ samples(1000),
      test("double") {
        check(Gen.double)(assertRoundtrips)
      } @@ samples(1000),
      test("float") {
        check(Gen.float)(assertRoundtrips)
      } @@ samples(1000),
      test("int") {
        check(Gen.int)(assertRoundtrips)
      } @@ samples(1000),
      test("long") {
        check(Gen.long)(assertRoundtrips)
      } @@ samples(1000),
      test("short") {
        check(Gen.short)(assertRoundtrips)
      } @@ samples(1000),
      test("string") {
        check(Gen.string)(assertRoundtrips)
      } @@ samples(1000)
    ),
    suite("java.time")(
      test("Duration") {
        check(genDuration)(assertRoundtrips)
      } @@ samples(1000),
      test("Instant") {
        check(genInstant)(assertRoundtrips)
      } @@ samples(1000),
      test("LocalDate") {
        check(genLocalDate)(assertRoundtrips)
      } @@ samples(1000),
      test("LocalDateTime") {
        check(genLocalDateTime)(assertRoundtrips)
      } @@ samples(1000),
      test("LocalTime") {
        check(genLocalTime)(assertRoundtrips)
      } @@ samples(1000),
      test("Month") {
        check(genMonth)(assertRoundtrips)
      } @@ samples(1000),
      test("MonthDay") {
        check(genMonthDay)(assertRoundtrips)
      } @@ samples(1000),
      test("OffsetDateTime") {
        check(genOffsetDateTime)(assertRoundtrips)
      } @@ samples(1000),
      test("OffsetTime") {
        check(genOffsetTime)(assertRoundtrips)
      } @@ samples(1000),
      test("Period") {
        check(genPeriod)(assertRoundtrips)
      } @@ samples(1000),
      test("Year") {
        check(genYear)(assertRoundtrips)
      } @@ samples(1000),
      test("YearMonth") {
        check(genYearMonth)(assertRoundtrips)
      } @@ samples(1000),
      test("ZoneId") {
        check(genZoneId)(assertRoundtrips[ZoneId])
      },
      test("ZoneOffset") {
        check(genZoneOffset)(assertRoundtrips[ZoneOffset])
      },
      test("ZonedDateTime") {
        check(genZonedDateTime)(assertRoundtrips)
      } @@ samples(1000)
    ),
    test("UUID") {
      check(Gen.uuid)(assertRoundtrips)
    } @@ samples(1000)
  )

  private def assertRoundtrips[A: SExprEncoder: SExprDecoder](a: A) =
    assert(a.toSExpr.fromSExpr[A])(isRight(equalTo(a))) &&
      assert(a.toSExprPretty.fromSExpr[A])(isRight(equalTo(a)))
}
