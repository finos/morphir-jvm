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
      testM("bigInt") {
        check(genBigInteger)(assertRoundtrips)
      } @@ samples(1000),
      testM("bigDecimal") {
        check(genBigDecimal)(assertRoundtrips)
      } @@ samples(1000),
      testM("boolean") {
        check(Gen.boolean)(assertRoundtrips)
      } @@ samples(1000),
      testM("byte") {
        check(Gen.anyByte)(assertRoundtrips)
      } @@ samples(1000),
      testM("char") {
        check(Gen.anyChar)(assertRoundtrips)
      } @@ samples(1000),
      testM("double") {
        check(Gen.anyDouble)(assertRoundtrips)
      } @@ samples(1000),
      testM("float") {
        check(Gen.anyFloat)(assertRoundtrips)
      } @@ samples(1000),
      testM("int") {
        check(Gen.anyInt)(assertRoundtrips)
      } @@ samples(1000),
      testM("long") {
        check(Gen.anyLong)(assertRoundtrips)
      } @@ samples(1000),
      testM("short") {
        check(Gen.anyShort)(assertRoundtrips)
      } @@ samples(1000),
      testM("string") {
        check(Gen.anyString)(assertRoundtrips)
      } @@ samples(1000)
    ),
    suite("java.time")(
      testM("Duration") {
        check(genDuration)(assertRoundtrips)
      } @@ samples(1000),
      testM("Instant") {
        check(genInstant)(assertRoundtrips)
      } @@ samples(1000),
      testM("LocalDate") {
        check(genLocalDate)(assertRoundtrips)
      } @@ samples(1000),
      testM("LocalDateTime") {
        check(genLocalDateTime)(assertRoundtrips)
      } @@ samples(1000),
      testM("LocalTime") {
        check(genLocalTime)(assertRoundtrips)
      } @@ samples(1000),
      testM("Month") {
        check(genMonth)(assertRoundtrips)
      } @@ samples(1000),
      testM("MonthDay") {
        check(genMonthDay)(assertRoundtrips)
      } @@ samples(1000),
      testM("OffsetDateTime") {
        check(genOffsetDateTime)(assertRoundtrips)
      } @@ samples(1000),
      testM("OffsetTime") {
        check(genOffsetTime)(assertRoundtrips)
      } @@ samples(1000),
      testM("Period") {
        check(genPeriod)(assertRoundtrips)
      } @@ samples(1000),
      testM("Year") {
        check(genYear)(assertRoundtrips)
      } @@ samples(1000),
      testM("YearMonth") {
        check(genYearMonth)(assertRoundtrips)
      } @@ samples(1000),
      testM("ZoneId") {
        check(genZoneId)(assertRoundtrips[ZoneId])
      },
      testM("ZoneOffset") {
        check(genZoneOffset)(assertRoundtrips[ZoneOffset])
      },
      testM("ZonedDateTime") {
        check(genZonedDateTime)(assertRoundtrips)
      } @@ samples(1000)
    ),
    testM("UUID") {
      check(Gen.anyUUID)(assertRoundtrips)
    } @@ samples(1000)
  )

  private def assertRoundtrips[A: SExprEncoder: SExprDecoder](a: A) =
    assert(a.toSExpr.fromSExpr[A])(isRight(equalTo(a))) &&
      assert(a.toSExprPretty.fromSExpr[A])(isRight(equalTo(a)))
}
