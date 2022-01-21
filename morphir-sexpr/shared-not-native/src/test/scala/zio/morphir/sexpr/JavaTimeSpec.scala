package zio.morphir.sexpr

import zio.test.Assertion._
import zio.test._
import zio.morphir.testing.ZioBaseSpec

import java.time._
import java.time.format.DateTimeFormatter

object JavaTimeSpec extends ZioBaseSpec {
  private def stringify(s: Any): String = s""""${s.toString}""""

  def spec: Spec[Annotations, TestFailure[Any], TestSuccess] =
    suite("java.time")(
      suite("Encoder")(
        test("DayOfWeek") {
          assertTrue(
            DayOfWeek.MONDAY.toSExpr == stringify("MONDAY"),
            DayOfWeek.TUESDAY.toSExpr == stringify("TUESDAY"),
            DayOfWeek.WEDNESDAY.toSExpr == stringify("WEDNESDAY"),
            DayOfWeek.THURSDAY.toSExpr == stringify("THURSDAY"),
            DayOfWeek.FRIDAY.toSExpr == stringify("FRIDAY"),
            DayOfWeek.SATURDAY.toSExpr == stringify("SATURDAY"),
            DayOfWeek.SUNDAY.toSExpr == stringify("SUNDAY")
          )
        },
        test("Duration") {
          assertTrue(
            Duration.ofDays(0).toSExpr == stringify("PT0S"),
            Duration.ofDays(1).toSExpr == stringify("PT24H"),
            Duration.ofHours(24).toSExpr == stringify("PT24H"),
            Duration.ofMinutes(1440).toSExpr == stringify("PT24H"),
            Duration.ofSeconds(Long.MaxValue, 999999999L).toSExpr == stringify("PT2562047788015215H30M7.999999999S")
          )
          // todo uncomment when decoder ready
//        //  &&
//        // assert(""""PT-0.5S"""".fromSExpr[Duration].map(_.toString))(isRight(equalTo("PT-0.5S")))
          // assert(""""-PT0.5S"""".fromSExpr[Duration].map(_.toString))(isRight(equalTo("PT-0.5S")))
        },
        test("Instant") {
          val n = Instant.now()
          assertTrue(Instant.EPOCH.toSExpr == stringify("1970-01-01T00:00:00Z"), n.toSExpr == stringify(n.toString))
        },
        test("LocalDate") {
          val n = LocalDate.now()
          val p = LocalDate.of(2020, 1, 1)

          assertTrue(
            n.toSExpr == stringify(n.format(DateTimeFormatter.ISO_LOCAL_DATE)),
            p.toSExpr == stringify("2020-01-01")
          )
        },
        test("LocalDateTime") {
          val n = LocalDateTime.now()
          val p = LocalDateTime.of(2020, 1, 1, 12, 36, 0)

          assertTrue(
            n.toSExpr == stringify(n.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)),
            p.toSExpr == stringify("2020-01-01T12:36:00")
          )
        },
        test("LocalTime") {
          val n = LocalTime.now()
          val p = LocalTime.of(12, 36, 0)

          assertTrue(
            n.toSExpr == stringify(n.format(DateTimeFormatter.ISO_LOCAL_TIME)),
            p.toSExpr == stringify("12:36:00")
          )
        },
        test("Month") {
          assertTrue(
            Month.JANUARY.toSExpr == stringify("JANUARY"),
            Month.FEBRUARY.toSExpr == stringify("FEBRUARY"),
            Month.MARCH.toSExpr == stringify("MARCH"),
            Month.APRIL.toSExpr == stringify("APRIL"),
            Month.MAY.toSExpr == stringify("MAY"),
            Month.JUNE.toSExpr == stringify("JUNE"),
            Month.JULY.toSExpr == stringify("JULY"),
            Month.AUGUST.toSExpr == stringify("AUGUST"),
            Month.SEPTEMBER.toSExpr == stringify("SEPTEMBER"),
            Month.OCTOBER.toSExpr == stringify("OCTOBER"),
            Month.NOVEMBER.toSExpr == stringify("NOVEMBER"),
            Month.DECEMBER.toSExpr == stringify("DECEMBER")
          )
        },
        test("MonthDay") {
          val n = MonthDay.now()
          val p = MonthDay.of(1, 1)

          assertTrue(n.toSExpr == stringify(n.toString), p.toSExpr == stringify("--01-01"))
        },
        test("OffsetDateTime") {
          val n = OffsetDateTime.now()
          val p = OffsetDateTime.of(2020, 1, 1, 12, 36, 12, 0, ZoneOffset.UTC)

          assertTrue(
            n.toSExpr == stringify(n.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)),
            p.toSExpr == stringify("2020-01-01T12:36:12Z")
          )
        },
        test("OffsetTime") {
          val n = OffsetTime.now()
          val p = OffsetTime.of(12, 36, 12, 0, ZoneOffset.ofHours(-4))

          assertTrue(
            n.toSExpr == stringify(n.format(DateTimeFormatter.ISO_OFFSET_TIME)),
            p.toSExpr == stringify("12:36:12-04:00")
          )
        },
        test("Period") {
          assertTrue(
            Period.ZERO.toSExpr == stringify("P0D"),
            Period.ofDays(1).toSExpr == stringify("P1D"),
            Period.ofMonths(2).toSExpr == stringify("P2M"),
            Period.ofWeeks(52).toSExpr == stringify("P364D"),
            Period.ofYears(10).toSExpr == stringify("P10Y")
          )
        },
        test("Year") {
          val n = Year.now()
          assertTrue(
            n.toSExpr == stringify(n.toString),
            Year.of(1999).toSExpr == stringify("1999"),
            Year.of(10000).toSExpr == stringify("+10000")
          )
        },
        test("YearMonth") {
          val n = YearMonth.now()
          assertTrue(
            n.toSExpr == stringify(n.toString),
            YearMonth.of(1999, 12).toSExpr == stringify("1999-12"),
            YearMonth.of(1999, 1).toSExpr == stringify("1999-01")
          )
        },
        test("ZonedDateTime") {
          val n   = ZonedDateTime.now()
          val ld  = LocalDateTime.of(2020, 1, 1, 12, 36, 0)
          val est = ZonedDateTime.of(ld, ZoneId.of("America/New_York"))
          val utc = ZonedDateTime.of(ld, ZoneId.of("Etc/UTC"))
          assertTrue(
            n.toSExpr == stringify(n.format(DateTimeFormatter.ISO_ZONED_DATE_TIME)),
            est.toSExpr == stringify("2020-01-01T12:36:00-05:00[America/New_York]"),
            utc.toSExpr == stringify("2020-01-01T12:36:00Z[Etc/UTC]")
          )
        },
        test("ZoneId") {
          assertTrue(
            ZoneId.of("America/New_York").toSExpr == stringify("America/New_York"),
            ZoneId.of("Etc/UTC").toSExpr == stringify("Etc/UTC"),
            ZoneId.of("Pacific/Auckland").toSExpr == stringify("Pacific/Auckland"),
            ZoneId.of("Asia/Shanghai").toSExpr == stringify("Asia/Shanghai"),
            ZoneId.of("Africa/Cairo").toSExpr == stringify("Africa/Cairo")
          )
        },
        test("ZoneOffset") {
          assertTrue(
            ZoneOffset.UTC.toSExpr == stringify("Z"),
            ZoneOffset.ofHours(5).toSExpr == stringify("+05:00"),
            ZoneOffset.ofHours(-5).toSExpr == stringify("-05:00")
          )
        }
      ),
      suite("Decoder")(
        test("DayOfWeek") {
          assert(stringify("MONDAY").fromSExpr[DayOfWeek])(isRight(equalTo(DayOfWeek.MONDAY))) &&
          assert(stringify("TUESDAY").fromSExpr[DayOfWeek])(isRight(equalTo(DayOfWeek.TUESDAY))) &&
          assert(stringify("WEDNESDAY").fromSExpr[DayOfWeek])(isRight(equalTo(DayOfWeek.WEDNESDAY))) &&
          assert(stringify("THURSDAY").fromSExpr[DayOfWeek])(isRight(equalTo(DayOfWeek.THURSDAY))) &&
          assert(stringify("FRIDAY").fromSExpr[DayOfWeek])(isRight(equalTo(DayOfWeek.FRIDAY))) &&
          assert(stringify("SATURDAY").fromSExpr[DayOfWeek])(isRight(equalTo(DayOfWeek.SATURDAY))) &&
          assert(stringify("SUNDAY").fromSExpr[DayOfWeek])(isRight(equalTo(DayOfWeek.SUNDAY))) &&
          assert(stringify("monday").fromSExpr[DayOfWeek])(
            isRight(equalTo(DayOfWeek.MONDAY))
          ) &&
          assert(stringify("MonDay").fromSExpr[DayOfWeek])(
            isRight(equalTo(DayOfWeek.MONDAY))
          )
        },
        test("Duration") {
          assert(stringify("PT24H").fromSExpr[Duration])(isRight(equalTo(Duration.ofHours(24)))) &&
          assert(stringify("-PT24H").fromSExpr[Duration])(isRight(equalTo(Duration.ofHours(-24)))) &&
          assert(stringify("P1D").fromSExpr[Duration])(isRight(equalTo(Duration.ofHours(24)))) &&
          assert(stringify("P1DT0H").fromSExpr[Duration])(isRight(equalTo(Duration.ofHours(24)))) &&
          assert(stringify("PT2562047788015215H30M7.999999999S").fromSExpr[Duration])(
            isRight(equalTo(Duration.ofSeconds(Long.MaxValue, 999999999L)))
          )
        },
        test("Instant") {
          val n = Instant.now()
          assert(stringify("1970-01-01T00:00:00Z").fromSExpr[Instant])(isRight(equalTo(Instant.EPOCH))) &&
          assert(stringify("1970-01-01T00:00:00.Z").fromSExpr[Instant])(isRight(equalTo(Instant.EPOCH))) &&
          assert(stringify(n).fromSExpr[Instant])(isRight(equalTo(n)))
        },
        test("LocalDate") {
          val n = LocalDate.now()
          val p = LocalDate.of(2000, 2, 29)
          assert(stringify(n).fromSExpr[LocalDate])(isRight(equalTo(n))) &&
          assert(stringify(p).fromSExpr[LocalDate])(isRight(equalTo(p)))
        },
        test("LocalDateTime") {
          val n = LocalDateTime.now()
          val p = LocalDateTime.of(2020, 1, 1, 12, 36, 0)
          assert(stringify(n).fromSExpr[LocalDateTime])(isRight(equalTo(n))) &&
          assert(stringify("2020-01-01T12:36").fromSExpr[LocalDateTime])(isRight(equalTo(p)))
          assert(stringify("2020-01-01T12:36:00.").fromSExpr[LocalDateTime])(isRight(equalTo(p)))
        },
        test("LocalTime") {
          val n = LocalTime.now()
          val p = LocalTime.of(12, 36, 0)
          assert(stringify(n).fromSExpr[LocalTime])(isRight(equalTo(n))) &&
          assert(stringify("12:36").fromSExpr[LocalTime])(isRight(equalTo(p)))
          assert(stringify("12:36:00.").fromSExpr[LocalTime])(isRight(equalTo(p)))
        },
        test("Month") {
          assert(stringify("JANUARY").fromSExpr[Month])(isRight(equalTo(Month.JANUARY))) &&
          assert(stringify("FEBRUARY").fromSExpr[Month])(isRight(equalTo(Month.FEBRUARY))) &&
          assert(stringify("MARCH").fromSExpr[Month])(isRight(equalTo(Month.MARCH))) &&
          assert(stringify("APRIL").fromSExpr[Month])(isRight(equalTo(Month.APRIL))) &&
          assert(stringify("MAY").fromSExpr[Month])(isRight(equalTo(Month.MAY))) &&
          assert(stringify("JUNE").fromSExpr[Month])(isRight(equalTo(Month.JUNE))) &&
          assert(stringify("JULY").fromSExpr[Month])(isRight(equalTo(Month.JULY))) &&
          assert(stringify("AUGUST").fromSExpr[Month])(isRight(equalTo(Month.AUGUST))) &&
          assert(stringify("SEPTEMBER").fromSExpr[Month])(isRight(equalTo(Month.SEPTEMBER))) &&
          assert(stringify("OCTOBER").fromSExpr[Month])(isRight(equalTo(Month.OCTOBER))) &&
          assert(stringify("NOVEMBER").fromSExpr[Month])(isRight(equalTo(Month.NOVEMBER))) &&
          assert(stringify("DECEMBER").fromSExpr[Month])(isRight(equalTo(Month.DECEMBER))) &&
          assert(stringify("december").fromSExpr[Month])(isRight(equalTo(Month.DECEMBER))) &&
          assert(stringify("December").fromSExpr[Month])(isRight(equalTo(Month.DECEMBER)))
        },
        test("MonthDay") {
          val n = MonthDay.now()
          val p = MonthDay.of(1, 1)
          assert(stringify(n).fromSExpr[MonthDay])(isRight(equalTo(n))) &&
          assert(stringify("--01-01").fromSExpr[MonthDay])(isRight(equalTo(p)))
        },
        test("OffsetDateTime") {
          val n = OffsetDateTime.now()
          val p = OffsetDateTime.of(2020, 1, 1, 12, 36, 12, 0, ZoneOffset.UTC)
          assert(stringify(n).fromSExpr[OffsetDateTime])(isRight(equalTo(n))) &&
          assert(stringify("2020-01-01T12:36:12Z").fromSExpr[OffsetDateTime])(isRight(equalTo(p)))
          assert(stringify("2020-01-01T12:36:12.Z").fromSExpr[OffsetDateTime])(isRight(equalTo(p)))
        },
        test("OffsetTime") {
          val n = OffsetTime.now()
          val p = OffsetTime.of(12, 36, 12, 0, ZoneOffset.ofHours(-4))
          assert(stringify(n).fromSExpr[OffsetTime])(isRight(equalTo(n))) &&
          assert(stringify("12:36:12-04:00").fromSExpr[OffsetTime])(isRight(equalTo(p)))
          assert(stringify("12:36:12.-04:00").fromSExpr[OffsetTime])(isRight(equalTo(p)))
        },
        test("Period") {
          assert(stringify("P0D").fromSExpr[Period])(isRight(equalTo(Period.ZERO))) &&
          assert(stringify("P1D").fromSExpr[Period])(isRight(equalTo(Period.ofDays(1)))) &&
          assert(stringify("P-1D").fromSExpr[Period])(isRight(equalTo(Period.ofDays(-1)))) &&
          assert(stringify("-P1D").fromSExpr[Period])(isRight(equalTo(Period.ofDays(-1)))) &&
          assert(stringify("P2M").fromSExpr[Period])(isRight(equalTo(Period.ofMonths(2)))) &&
          assert(stringify("P364D").fromSExpr[Period])(isRight(equalTo(Period.ofWeeks(52)))) &&
          assert(stringify("P10Y").fromSExpr[Period])(isRight(equalTo(Period.ofYears(10))))
        },
        test("Year") {
          val n = Year.now()
          assert(stringify(n).fromSExpr[Year])(isRight(equalTo(n))) &&
          assert(stringify("1999").fromSExpr[Year])(isRight(equalTo(Year.of(1999)))) &&
          assert(stringify("+10000").fromSExpr[Year])(isRight(equalTo(Year.of(10000))))
        },
        test("YearMonth") {
          val n = YearMonth.now()
          assert(stringify(n).fromSExpr[YearMonth])(isRight(equalTo(n))) &&
          assert(stringify("1999-12").fromSExpr[YearMonth])(isRight(equalTo(YearMonth.of(1999, 12)))) &&
          assert(stringify("1999-01").fromSExpr[YearMonth])(isRight(equalTo(YearMonth.of(1999, 1))))
        },
        test("ZonedDateTime") {
          def zdtAssert(actual: String, expected: ZonedDateTime): TestResult =
            assert(stringify(actual).fromSExpr[ZonedDateTime].map(_.toString))(isRight(equalTo(expected.toString)))

          val n   = ZonedDateTime.now()
          val ld  = LocalDateTime.of(2020, 1, 1, 12, 36, 0)
          val est = ZonedDateTime.of(ld, ZoneId.of("America/New_York"))
          val utc = ZonedDateTime.of(ld, ZoneId.of("Etc/UTC"))
          val gmt = ZonedDateTime.of(ld, ZoneId.of("+00:00"))

          zdtAssert(n.toString, n) &&
          zdtAssert("2020-01-01T12:36:00-05:00[America/New_York]", est) &&
          zdtAssert("2020-01-01T12:36:00Z[Etc/UTC]", utc) &&
          zdtAssert("2020-01-01T12:36:00.Z[Etc/UTC]", utc) &&
          zdtAssert("2020-01-01T12:36:00+00:00[+00:00]", gmt) &&
          zdtAssert(
            "2018-02-01T00:00Z",
            ZonedDateTime.of(LocalDateTime.of(2018, 2, 1, 0, 0, 0), ZoneOffset.UTC)
          ) &&
          zdtAssert(
            "2018-03-01T00:00:00Z",
            ZonedDateTime.of(LocalDateTime.of(2018, 3, 1, 0, 0, 0), ZoneOffset.UTC)
          ) &&
          zdtAssert(
            "2018-04-01T00:00:00.000Z",
            ZonedDateTime.of(LocalDateTime.of(2018, 4, 1, 0, 0, 0), ZoneOffset.UTC)
          ) &&
          zdtAssert(
            "+999999999-12-31T23:59:59.999999999+18:00",
            ZonedDateTime.of(LocalDateTime.MAX, ZoneOffset.MAX)
          ) &&
          zdtAssert(
            "+999999999-12-31T23:59:59.999999999-18:00",
            ZonedDateTime.of(LocalDateTime.MAX, ZoneOffset.MIN)
          ) &&
          zdtAssert("-999999999-01-01T00:00:00+18:00", ZonedDateTime.of(LocalDateTime.MIN, ZoneOffset.MAX)) &&
          zdtAssert("-999999999-01-01T00:00:00-18:00", ZonedDateTime.of(LocalDateTime.MIN, ZoneOffset.MIN)) &&
          zdtAssert(
            "2012-10-28T02:00:00+01:00[Europe/Berlin]",
            OffsetDateTime.parse("2012-10-28T02:00:00+01:00").atZoneSameInstant(ZoneId.of("Europe/Berlin"))
          ) &&
          zdtAssert(
            "2018-03-25T02:30+01:00[Europe/Warsaw]",
            ZonedDateTime.parse("2018-03-25T02:30+01:00[Europe/Warsaw]")
          ) &&
          zdtAssert(
            "2018-03-25T02:30+00:00[Europe/Warsaw]",
            OffsetDateTime.parse("2018-03-25T02:30+00:00").atZoneSameInstant(ZoneId.of("Europe/Warsaw"))
          ) &&
          zdtAssert(
            "2018-03-25T02:30+02:00[Europe/Warsaw]",
            OffsetDateTime.parse("2018-03-25T02:30+02:00").atZoneSameInstant(ZoneId.of("Europe/Warsaw"))
          ) &&
          zdtAssert(
            "2018-03-25T02:30+03:00[Europe/Warsaw]",
            OffsetDateTime.parse("2018-03-25T02:30+03:00").atZoneSameInstant(ZoneId.of("Europe/Warsaw"))
          ) &&
          zdtAssert(
            "2018-10-28T02:30+00:00[Europe/Warsaw]",
            OffsetDateTime.parse("2018-10-28T02:30+00:00").atZoneSameInstant(ZoneId.of("Europe/Warsaw"))
          ) &&
          zdtAssert(
            "2018-10-28T02:30+01:00[Europe/Warsaw]",
            OffsetDateTime.parse("2018-10-28T02:30+01:00").atZoneSameInstant(ZoneId.of("Europe/Warsaw"))
          ) &&
          zdtAssert(
            "2018-10-28T02:30+02:00[Europe/Warsaw]",
            OffsetDateTime.parse("2018-10-28T02:30+02:00").atZoneSameInstant(ZoneId.of("Europe/Warsaw"))
          ) &&
          zdtAssert(
            "2018-10-28T02:30+03:00[Europe/Warsaw]",
            OffsetDateTime.parse("2018-10-28T02:30+03:00").atZoneSameInstant(ZoneId.of("Europe/Warsaw"))
          )
        },
        test("ZoneId") {
          assert(stringify("America/New_York").fromSExpr[ZoneId])(
            isRight(
              equalTo(
                ZoneId.of("America/New_York")
              )
            )
          ) &&
          assert(stringify("Etc/UTC").fromSExpr[ZoneId])(isRight(equalTo(ZoneId.of("Etc/UTC")))) &&
          assert(stringify("Pacific/Auckland").fromSExpr[ZoneId])(
            isRight(
              equalTo(
                ZoneId.of("Pacific/Auckland")
              )
            )
          ) &&
          assert(stringify("Asia/Shanghai").fromSExpr[ZoneId])(
            isRight(equalTo(ZoneId.of("Asia/Shanghai")))
          ) &&
          assert(stringify("Africa/Cairo").fromSExpr[ZoneId])(isRight(equalTo(ZoneId.of("Africa/Cairo"))))
        },
        test("ZoneOffset") {
          assert(stringify("Z").fromSExpr[ZoneOffset])(isRight(equalTo(ZoneOffset.UTC))) &&
          assert(stringify("+05:00").fromSExpr[ZoneOffset])(isRight(equalTo(ZoneOffset.ofHours(5)))) &&
          assert(stringify("-05:00").fromSExpr[ZoneOffset])(isRight(equalTo(ZoneOffset.ofHours(-5))))
        }
      ),
      suite("Decoder Sad Path")(
        test("DayOfWeek") {
          assert(stringify("foody").fromSExpr[DayOfWeek])(
            isLeft(
              equalTo("(No enum constant java.time.DayOfWeek.FOODY)") || // JVM
                equalTo("(Unrecognized day of week name: FOODY)") ||
                equalTo("(enum case not found: FOODY)")
            ) // Scala.js
          )
        },
        test("Duration") {
          assert("""""""".fromSExpr[Duration])(
            isLeft(containsString(" is not a valid ISO-8601 format, illegal duration at index 0"))
          ) &&
          assert(""""X"""".fromSExpr[Duration])(
            isLeft(containsString("X is not a valid ISO-8601 format, expected 'P' or '-' at index 0"))
          ) &&
          assert(""""P"""".fromSExpr[Duration])(
            isLeft(containsString("P is not a valid ISO-8601 format, illegal duration at index 1"))
          ) &&
          assert(""""-"""".fromSExpr[Duration])(
            isLeft(containsString("- is not a valid ISO-8601 format, illegal duration at index 1"))
          ) &&
          assert(""""-X"""".fromSExpr[Duration])(
            isLeft(containsString("-X is not a valid ISO-8601 format, expected 'P' at index 1"))
          ) &&
          assert(""""PXD"""".fromSExpr[Duration])(
            isLeft(containsString("PXD is not a valid ISO-8601 format, expected '-' or digit at index 1"))
          ) &&
          assert(""""P-"""".fromSExpr[Duration])(
            isLeft(containsString("P- is not a valid ISO-8601 format, illegal duration at index 2"))
          ) &&
          assert(""""P-XD"""".fromSExpr[Duration])(
            isLeft(containsString("P-XD is not a valid ISO-8601 format, expected digit at index 2"))
          ) &&
          assert(""""P1XD"""".fromSExpr[Duration])(
            isLeft(containsString("P1XD is not a valid ISO-8601 format, expected 'D' or digit at index 2"))
          ) &&
          assert(""""PT"""".fromSExpr[Duration])(
            isLeft(containsString("PT is not a valid ISO-8601 format, illegal duration at index 2"))
          ) &&
          assert(""""PT0SX"""".fromSExpr[Duration])(
            isLeft(containsString("PT0SX is not a valid ISO-8601 format, illegal duration at index 4"))
          ) &&
          assert(""""P1DT"""".fromSExpr[Duration])(
            isLeft(containsString("P1DT is not a valid ISO-8601 format, illegal duration at index 4"))
          ) &&
          assert(""""P106751991167301D"""".fromSExpr[Duration])(
            isLeft(containsString("P106751991167301D is not a valid ISO-8601 format, illegal duration at index 16"))
          ) &&
          assert(""""P1067519911673000D"""".fromSExpr[Duration])(
            isLeft(containsString("P1067519911673000D is not a valid ISO-8601 format, illegal duration at index 17"))
          ) &&
          assert(""""P-106751991167301D"""".fromSExpr[Duration])(
            isLeft(containsString("P-106751991167301D is not a valid ISO-8601 format, illegal duration at index 17"))
          ) &&
          assert(""""P1DX1H"""".fromSExpr[Duration])(
            isLeft(containsString("P1DX1H is not a valid ISO-8601 format, expected 'T' or '\"' at index 3"))
          ) &&
          assert(""""P1DTXH"""".fromSExpr[Duration])(
            isLeft(containsString("P1DTXH is not a valid ISO-8601 format, expected '-' or digit at index 4"))
          ) &&
          assert(""""P1DT-XH"""".fromSExpr[Duration])(
            isLeft(containsString("P1DT-XH is not a valid ISO-8601 format, expected digit at index 5"))
          ) &&
          assert(""""P1DT1XH"""".fromSExpr[Duration])(
            isLeft(
              containsString(
                "P1DT1XH is not a valid ISO-8601 format, expected 'H' or 'M' or 'S or '.' or digit at index 5"
              )
            )
          ) &&
          assert(""""P1DT1H1XM"""".fromSExpr[Duration])(
            isLeft(
              containsString("P1DT1H1XM is not a valid ISO-8601 format, expected 'M' or 'S or '.' or digit at index 7")
            )
          ) &&
          assert(""""P0DT2562047788015216H"""".fromSExpr[Duration])(
            isLeft(containsString("P0DT2562047788015216H is not a valid ISO-8601 format, illegal duration at index 20"))
          ) &&
          assert(""""P0DT-2562047788015216H"""".fromSExpr[Duration])(
            isLeft(
              containsString("P0DT-2562047788015216H is not a valid ISO-8601 format, illegal duration at index 21")
            )
          ) &&
          assert(""""P0DT153722867280912931M"""".fromSExpr[Duration])(
            isLeft(
              containsString("P0DT153722867280912931M is not a valid ISO-8601 format, illegal duration at index 22")
            )
          ) &&
          assert(""""P0DT-153722867280912931M"""".fromSExpr[Duration])(
            isLeft(
              containsString("P0DT-153722867280912931M is not a valid ISO-8601 format, illegal duration at index 23")
            )
          ) &&
          assert(""""P0DT9223372036854775808S"""".fromSExpr[Duration])(
            isLeft(
              containsString("P0DT9223372036854775808S is not a valid ISO-8601 format, illegal duration at index 23")
            )
          ) &&
          assert(""""P0DT92233720368547758000S"""".fromSExpr[Duration])(
            isLeft(
              containsString("P0DT92233720368547758000S is not a valid ISO-8601 format, illegal duration at index 23")
            )
          ) &&
          assert(""""P0DT-9223372036854775809S"""".fromSExpr[Duration])(
            isLeft(
              containsString("P0DT-9223372036854775809S is not a valid ISO-8601 format, illegal duration at index 23")
            )
          ) &&
          assert(""""P1DT1H1MXS"""".fromSExpr[Duration])(
            isLeft(
              containsString("P1DT1H1MXS is not a valid ISO-8601 format, expected '\"' or '-' or digit at index 8")
            )
          ) &&
          assert(""""P1DT1H1M-XS"""".fromSExpr[Duration])(
            isLeft(containsString("P1DT1H1M-XS is not a valid ISO-8601 format, expected digit at index 9"))
          ) &&
          assert(""""P1DT1H1M0XS"""".fromSExpr[Duration])(
            isLeft(containsString("P1DT1H1M0XS is not a valid ISO-8601 format, expected 'S or '.' or digit at index 9"))
          ) &&
          assert(""""P1DT1H1M0.XS"""".fromSExpr[Duration])(
            isLeft(containsString("P1DT1H1M0.XS is not a valid ISO-8601 format, expected 'S' or digit at index 10"))
          ) &&
          assert(""""P1DT1H1M0.012345678XS"""".fromSExpr[Duration])(
            isLeft(containsString("P1DT1H1M0.012345678XS is not a valid ISO-8601 format, expected 'S' at index 19"))
          ) &&
          assert(""""P1DT1H1M0.0123456789S"""".fromSExpr[Duration])(
            isLeft(containsString("P1DT1H1M0.0123456789S is not a valid ISO-8601 format, expected 'S' at index 19"))
          ) &&
          assert(""""P0DT0H0M9223372036854775808S"""".fromSExpr[Duration])(
            isLeft(
              containsString(
                "P0DT0H0M9223372036854775808S is not a valid ISO-8601 format, illegal duration at index 27"
              )
            )
          ) &&
          assert(""""P0DT0H0M92233720368547758080S"""".fromSExpr[Duration])(
            isLeft(
              containsString(
                "P0DT0H0M92233720368547758080S is not a valid ISO-8601 format, illegal duration at index 27"
              )
            )
          ) &&
          assert(""""P0DT0H0M-9223372036854775809S"""".fromSExpr[Duration])(
            isLeft(
              containsString(
                "P0DT0H0M-9223372036854775809S is not a valid ISO-8601 format, illegal duration at index 27"
              )
            )
          ) &&
          assert(""""P106751991167300DT24H"""".fromSExpr[Duration])(
            isLeft(containsString("P106751991167300DT24H is not a valid ISO-8601 format, illegal duration at index 20"))
          ) &&
          assert(""""P0DT2562047788015215H60M"""".fromSExpr[Duration])(
            isLeft(
              containsString("P0DT2562047788015215H60M is not a valid ISO-8601 format, illegal duration at index 23")
            )
          ) &&
          assert(""""P0DT0H153722867280912930M60S"""".fromSExpr[Duration])(
            isLeft(
              containsString(
                "P0DT0H153722867280912930M60S is not a valid ISO-8601 format, illegal duration at index 27"
              )
            )
          )
        },
        test("Instant") {
          assert(stringify("").fromSExpr[Instant])(
            isLeft(
              equalTo("( is not a valid ISO-8601 format, illegal instant at index 0)")
            )
          ) &&
          assert(stringify("2020").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020 is not a valid ISO-8601 format, illegal instant at index 0)")
            )
          ) &&
          assert(stringify("2020-0").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-0 is not a valid ISO-8601 format, illegal instant at index 5)")
            )
          ) &&
          assert(stringify("2020-01-0").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-01-0 is not a valid ISO-8601 format, illegal instant at index 8)")
            )
          ) &&
          assert(stringify("2020-01-01T0").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-01-01T0 is not a valid ISO-8601 format, illegal instant at index 11)")
            )
          ) &&
          assert(stringify("2020-01-01T01:0").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-01-01T01:0 is not a valid ISO-8601 format, illegal instant at index 14)")
            )
          ) &&
          assert(stringify("X020-01-01T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(X020-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or '+' or digit at index 0)")
            )
          ) &&
          assert(stringify("2X20-01-01T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(2X20-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("20X0-01-01T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(20X0-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("202X-01-01T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(202X-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("2020X01-01T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020X01-01T01:01Z is not a valid ISO-8601 format, expected '-' at index 4)")
            )
          ) &&
          assert(stringify("2020-X1-01T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-X1-01T01:01Z is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("2020-0X-01T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-0X-01T01:01Z is not a valid ISO-8601 format, expected digit at index 6)")
            )
          ) &&
          assert(stringify("2020-01X01T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-01X01T01:01Z is not a valid ISO-8601 format, expected '-' at index 7)")
            )
          ) &&
          assert(stringify("2020-01-X1T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-01-X1T01:01Z is not a valid ISO-8601 format, expected digit at index 8)")
            )
          ) &&
          assert(stringify("2020-01-0XT01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-01-0XT01:01Z is not a valid ISO-8601 format, expected digit at index 9)")
            )
          ) &&
          assert(stringify("2020-01-01X01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-01-01X01:01Z is not a valid ISO-8601 format, expected 'T' at index 10)")
            )
          ) &&
          assert(stringify("2020-01-01TX1:01").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-01-01TX1:01 is not a valid ISO-8601 format, expected digit at index 11)")
            )
          ) &&
          assert(stringify("2020-01-01T0X:01").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-01-01T0X:01 is not a valid ISO-8601 format, expected digit at index 12)")
            )
          ) &&
          assert(stringify("2020-01-01T24:01").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-01-01T24:01 is not a valid ISO-8601 format, illegal hour at index 12)")
            )
          ) &&
          assert(stringify("2020-01-01T01X01").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-01-01T01X01 is not a valid ISO-8601 format, expected ':' at index 13)")
            )
          ) &&
          assert(stringify("2020-01-01T01:X1").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-01-01T01:X1 is not a valid ISO-8601 format, expected digit at index 14)")
            )
          ) &&
          assert(stringify("2020-01-01T01:0X").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-01-01T01:0X is not a valid ISO-8601 format, expected digit at index 15)")
            )
          ) &&
          assert(stringify("2020-01-01T01:60").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-01-01T01:60 is not a valid ISO-8601 format, illegal minute at index 15)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01X").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-01-01T01:01X is not a valid ISO-8601 format, expected ':' or 'Z' at index 16)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:0").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-01-01T01:01:0 is not a valid ISO-8601 format, illegal instant at index 17)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:X1Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-01-01T01:01:X1Z is not a valid ISO-8601 format, expected digit at index 17)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:0XZ").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-01-01T01:01:0XZ is not a valid ISO-8601 format, expected digit at index 18)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:60Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-01-01T01:01:60Z is not a valid ISO-8601 format, illegal second at index 18)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:012").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-01-01T01:01:012 is not a valid ISO-8601 format, expected '.' or 'Z' at index 19)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01.X").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-01-01T01:01:01.X is not a valid ISO-8601 format, expected digit or 'Z' at index 20)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01.123456789X").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-01-01T01:01:01.123456789X is not a valid ISO-8601 format, expected 'Z' at index 29)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01ZX").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-01-01T01:01:01ZX is not a valid ISO-8601 format, illegal instant at index 20)")
            )
          ) &&
          assert(stringify("+X0000-01-01T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(+X0000-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("+1X000-01-01T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(+1X000-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("+10X00-01-01T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(+10X00-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("+100X0-01-01T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(+100X0-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 4)")
            )
          ) &&
          assert(stringify("+1000X-01-01T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(+1000X-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("+10000X-01-01T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(+10000X-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or digit at index 6)")
            )
          ) &&
          assert(stringify("+100000X-01-01T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(+100000X-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or digit at index 7)")
            )
          ) &&
          assert(stringify("+1000000X-01-01T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(+1000000X-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or digit at index 8)")
            )
          ) &&
          assert(stringify("+1000000001-01-01T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(+1000000001-01-01T01:01Z is not a valid ISO-8601 format, illegal year at index 10)")
            )
          ) &&
          assert(stringify("+3333333333-01-01T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(+3333333333-01-01T01:01Z is not a valid ISO-8601 format, illegal year at index 10)")
            )
          ) &&
          assert(stringify("-1000000001-01-01T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(-1000000001-01-01T01:01Z is not a valid ISO-8601 format, illegal year at index 10)")
            )
          ) &&
          assert(stringify("-0000-01-01T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(-0000-01-01T01:01Z is not a valid ISO-8601 format, illegal year at index 4)")
            )
          ) &&
          assert(stringify("+10000").fromSExpr[Instant])(
            isLeft(
              equalTo("(+10000 is not a valid ISO-8601 format, illegal instant at index 6)")
            )
          ) &&
          assert(stringify("2020-00-01T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-00-01T01:01Z is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-13-01T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-13-01T01:01Z is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-01-00T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-01-00T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-01-32T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-01-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-02-30T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-02-30T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-03-32T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-03-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-04-31T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-04-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-05-32T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-05-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-06-31T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-06-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-07-32T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-07-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-08-32T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-08-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-09-31T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-09-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-10-32T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-10-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-11-31T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-11-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-12-32T01:01Z").fromSExpr[Instant])(
            isLeft(
              equalTo("(2020-12-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          )
        },
        test("LocalDate") {
          assert(stringify("").fromSExpr[LocalDate])(
            isLeft(
              equalTo("( is not a valid ISO-8601 format, illegal local date at index 0)")
            )
          ) &&
          assert(stringify("2020").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(2020 is not a valid ISO-8601 format, illegal local date at index 0)")
            )
          ) &&
          assert(stringify("2020-0").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(2020-0 is not a valid ISO-8601 format, illegal local date at index 5)")
            )
          ) &&
          assert(stringify("2020-01-0").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(2020-01-0 is not a valid ISO-8601 format, illegal local date at index 8)")
            )
          ) &&
          assert(stringify("2020-01-012").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(2020-01-012 is not a valid ISO-8601 format, illegal local date at index 10)")
            )
          ) &&
          assert(stringify("X020-01-01").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(X020-01-01 is not a valid ISO-8601 format, expected '-' or '+' or digit at index 0)")
            )
          ) &&
          assert(stringify("2X20-01-01").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(2X20-01-01 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("20X0-01-01").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(20X0-01-01 is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("202X-01-01").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(202X-01-01 is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("2020X01-01").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(2020X01-01 is not a valid ISO-8601 format, expected '-' at index 4)")
            )
          ) &&
          assert(stringify("2020-X1-01").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(2020-X1-01 is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("2020-0X-01").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(2020-0X-01 is not a valid ISO-8601 format, expected digit at index 6)")
            )
          ) &&
          assert(stringify("2020-01X01").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(2020-01X01 is not a valid ISO-8601 format, expected '-' at index 7)")
            )
          ) &&
          assert(stringify("2020-01-X1").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(2020-01-X1 is not a valid ISO-8601 format, expected digit at index 8)")
            )
          ) &&
          assert(stringify("2020-01-0X").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(2020-01-0X is not a valid ISO-8601 format, expected digit at index 9)")
            )
          ) &&
          assert(stringify("+X0000-01-01").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(+X0000-01-01 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("+1X000-01-01").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(+1X000-01-01 is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("+10X00-01-01").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(+10X00-01-01 is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("+100X0-01-01").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(+100X0-01-01 is not a valid ISO-8601 format, expected digit at index 4)")
            )
          ) &&
          assert(stringify("+1000X-01-01").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(+1000X-01-01 is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("+10000X-01-01").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(+10000X-01-01 is not a valid ISO-8601 format, expected '-' or digit at index 6)")
            )
          ) &&
          assert(stringify("+100000X-01-01").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(+100000X-01-01 is not a valid ISO-8601 format, expected '-' or digit at index 7)")
            )
          ) &&
          assert(stringify("+1000000X-01-01").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(+1000000X-01-01 is not a valid ISO-8601 format, expected '-' or digit at index 8)")
            )
          ) &&
          assert(stringify("+1000000000-01-01").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(+1000000000-01-01 is not a valid ISO-8601 format, expected '-' at index 10)")
            )
          ) &&
          assert(stringify("-1000000000-01-01").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(-1000000000-01-01 is not a valid ISO-8601 format, expected '-' at index 10)")
            )
          ) &&
          assert(stringify("-0000-01-01").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(-0000-01-01 is not a valid ISO-8601 format, illegal year at index 4)")
            )
          ) &&
          assert(stringify("+10000").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(+10000 is not a valid ISO-8601 format, illegal local date at index 6)")
            )
          ) &&
          assert(stringify("2020-00-01").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(2020-00-01 is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-13-01").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(2020-13-01 is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-01-00").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(2020-01-00 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-01-32").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(2020-01-32 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-02-30").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(2020-02-30 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-03-32").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(2020-03-32 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-04-31").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(2020-04-31 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-05-32").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(2020-05-32 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-06-31").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(2020-06-31 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-07-32").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(2020-07-32 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-08-32").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(2020-08-32 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-09-31").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(2020-09-31 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-10-32").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(2020-10-32 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-11-31").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(2020-11-31 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-12-32").fromSExpr[LocalDate])(
            isLeft(
              equalTo("(2020-12-32 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          )
        },
        test("LocalDateTime") {
          assert(stringify("").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("( is not a valid ISO-8601 format, illegal local date time at index 0)")
            )
          ) &&
          assert(stringify("2020").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020 is not a valid ISO-8601 format, illegal local date time at index 0)")
            )
          ) &&
          assert(stringify("2020-0").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-0 is not a valid ISO-8601 format, illegal local date time at index 5)")
            )
          ) &&
          assert(stringify("2020-01-0").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-0 is not a valid ISO-8601 format, illegal local date time at index 8)")
            )
          ) &&
          assert(stringify("2020-01-01T0").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01T0 is not a valid ISO-8601 format, illegal local date time at index 11)")
            )
          ) &&
          assert(stringify("2020-01-01T01:0").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:0 is not a valid ISO-8601 format, illegal local date time at index 14)")
            )
          ) &&
          assert(stringify("X020-01-01T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(X020-01-01T01:01 is not a valid ISO-8601 format, expected '-' or '+' or digit at index 0)")
            )
          ) &&
          assert(stringify("2X20-01-01T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2X20-01-01T01:01 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("20X0-01-01T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(20X0-01-01T01:01 is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("202X-01-01T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(202X-01-01T01:01 is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("2020X01-01T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020X01-01T01:01 is not a valid ISO-8601 format, expected '-' at index 4)")
            )
          ) &&
          assert(stringify("2020-X1-01T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-X1-01T01:01 is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("2020-0X-01T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-0X-01T01:01 is not a valid ISO-8601 format, expected digit at index 6)")
            )
          ) &&
          assert(stringify("2020-01X01T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-01X01T01:01 is not a valid ISO-8601 format, expected '-' at index 7)")
            )
          ) &&
          assert(stringify("2020-01-X1T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-X1T01:01 is not a valid ISO-8601 format, expected digit at index 8)")
            )
          ) &&
          assert(stringify("2020-01-0XT01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-0XT01:01 is not a valid ISO-8601 format, expected digit at index 9)")
            )
          ) &&
          assert(stringify("2020-01-01X01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01X01:01 is not a valid ISO-8601 format, expected 'T' at index 10)")
            )
          ) &&
          assert(stringify("2020-01-01TX1:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01TX1:01 is not a valid ISO-8601 format, expected digit at index 11)")
            )
          ) &&
          assert(stringify("2020-01-01T0X:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01T0X:01 is not a valid ISO-8601 format, expected digit at index 12)")
            )
          ) &&
          assert(stringify("2020-01-01T24:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01T24:01 is not a valid ISO-8601 format, illegal hour at index 12)")
            )
          ) &&
          assert(stringify("2020-01-01T01X01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01T01X01 is not a valid ISO-8601 format, expected ':' at index 13)")
            )
          ) &&
          assert(stringify("2020-01-01T01:X1").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:X1 is not a valid ISO-8601 format, expected digit at index 14)")
            )
          ) &&
          assert(stringify("2020-01-01T01:0X").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:0X is not a valid ISO-8601 format, expected digit at index 15)")
            )
          ) &&
          assert(stringify("2020-01-01T01:60").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:60 is not a valid ISO-8601 format, illegal minute at index 15)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01X").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01X is not a valid ISO-8601 format, expected ':' at index 16)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:0").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:0 is not a valid ISO-8601 format, illegal local date time at index 17)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:X1").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:X1 is not a valid ISO-8601 format, expected digit at index 17)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:0X").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:0X is not a valid ISO-8601 format, expected digit at index 18)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:60").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:60 is not a valid ISO-8601 format, illegal second at index 18)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:012").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:012 is not a valid ISO-8601 format, expected '.' at index 19)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01.X").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01.X is not a valid ISO-8601 format, illegal local date time at index 20)")
            )
          ) &&
          assert(stringify("+X0000-01-01T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(+X0000-01-01T01:01 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("+1X000-01-01T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(+1X000-01-01T01:01 is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("+10X00-01-01T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(+10X00-01-01T01:01 is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("+100X0-01-01T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(+100X0-01-01T01:01 is not a valid ISO-8601 format, expected digit at index 4)")
            )
          ) &&
          assert(stringify("+1000X-01-01T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(+1000X-01-01T01:01 is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("+10000X-01-01T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(+10000X-01-01T01:01 is not a valid ISO-8601 format, expected '-' or digit at index 6)")
            )
          ) &&
          assert(stringify("+100000X-01-01T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(+100000X-01-01T01:01 is not a valid ISO-8601 format, expected '-' or digit at index 7)")
            )
          ) &&
          assert(stringify("+1000000X-01-01T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(+1000000X-01-01T01:01 is not a valid ISO-8601 format, expected '-' or digit at index 8)")
            )
          ) &&
          assert(stringify("+1000000000-01-01T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(+1000000000-01-01T01:01 is not a valid ISO-8601 format, expected '-' at index 10)")
            )
          ) &&
          assert(stringify("-1000000000-01-01T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(-1000000000-01-01T01:01 is not a valid ISO-8601 format, expected '-' at index 10)")
            )
          ) &&
          assert(stringify("-0000-01-01T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(-0000-01-01T01:01 is not a valid ISO-8601 format, illegal year at index 4)")
            )
          ) &&
          assert(stringify("+10000").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(+10000 is not a valid ISO-8601 format, illegal local date time at index 6)")
            )
          ) &&
          assert(stringify("2020-00-01T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-00-01T01:01 is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-13-01T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-13-01T01:01 is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-01-00T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-00T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-01-32T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-01-32T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-02-30T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-02-30T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-03-32T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-03-32T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-04-31T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-04-31T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-05-32T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-05-32T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-06-31T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-06-31T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-07-32T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-07-32T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-08-32T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-08-32T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-09-31T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-09-31T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-10-32T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-10-32T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-11-31T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-11-31T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-12-32T01:01").fromSExpr[LocalDateTime])(
            isLeft(
              equalTo("(2020-12-32T01:01 is not a valid ISO-8601 format, illegal day at index 9)")
            )
          )
        },
        test("LocalTime") {
          assert(stringify("").fromSExpr[LocalTime])(
            isLeft(
              equalTo("( is not a valid ISO-8601 format, illegal local time at index 0)")
            )
          ) &&
          assert(stringify("0").fromSExpr[LocalTime])(
            isLeft(
              equalTo("(0 is not a valid ISO-8601 format, illegal local time at index 0)")
            )
          ) &&
          assert(stringify("01:0").fromSExpr[LocalTime])(
            isLeft(
              equalTo("(01:0 is not a valid ISO-8601 format, illegal local time at index 3)")
            )
          ) &&
          assert(stringify("X1:01").fromSExpr[LocalTime])(
            isLeft(
              equalTo("(X1:01 is not a valid ISO-8601 format, expected digit at index 0)")
            )
          ) &&
          assert(stringify("0X:01").fromSExpr[LocalTime])(
            isLeft(
              equalTo("(0X:01 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("24:01").fromSExpr[LocalTime])(
            isLeft(
              equalTo("(24:01 is not a valid ISO-8601 format, illegal hour at index 1)")
            )
          ) &&
          assert(stringify("01X01").fromSExpr[LocalTime])(
            isLeft(
              equalTo("(01X01 is not a valid ISO-8601 format, expected ':' at index 2)")
            )
          ) &&
          assert(stringify("01:X1").fromSExpr[LocalTime])(
            isLeft(
              equalTo("(01:X1 is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("01:0X").fromSExpr[LocalTime])(
            isLeft(
              equalTo("(01:0X is not a valid ISO-8601 format, expected digit at index 4)")
            )
          ) &&
          assert(stringify("01:60").fromSExpr[LocalTime])(
            isLeft(
              equalTo("(01:60 is not a valid ISO-8601 format, illegal minute at index 4)")
            )
          ) &&
          assert(stringify("01:01X").fromSExpr[LocalTime])(
            isLeft(
              equalTo("(01:01X is not a valid ISO-8601 format, expected ':' at index 5)")
            )
          ) &&
          assert(stringify("01:01:0").fromSExpr[LocalTime])(
            isLeft(
              equalTo("(01:01:0 is not a valid ISO-8601 format, illegal local time at index 6)")
            )
          ) &&
          assert(stringify("01:01:X1").fromSExpr[LocalTime])(
            isLeft(
              equalTo("(01:01:X1 is not a valid ISO-8601 format, expected digit at index 6)")
            )
          ) &&
          assert(stringify("01:01:0X").fromSExpr[LocalTime])(
            isLeft(
              equalTo("(01:01:0X is not a valid ISO-8601 format, expected digit at index 7)")
            )
          ) &&
          assert(stringify("01:01:60").fromSExpr[LocalTime])(
            isLeft(
              equalTo("(01:01:60 is not a valid ISO-8601 format, illegal second at index 7)")
            )
          ) &&
          assert(stringify("01:01:012").fromSExpr[LocalTime])(
            isLeft(
              equalTo("(01:01:012 is not a valid ISO-8601 format, expected '.' at index 8)")
            )
          ) &&
          assert(stringify("01:01:01.X").fromSExpr[LocalTime])(
            isLeft(
              equalTo("(01:01:01.X is not a valid ISO-8601 format, illegal local time at index 9)")
            )
          )
        },
        test("Month") {
          assert(stringify("FebTober").fromSExpr[Month])(
            isLeft(
              equalTo("(No enum constant java.time.Month.FEBTOBER)") || // JVM
                equalTo("(Unrecognized month name: FEBTOBER)") ||
                equalTo("(enum case not found: FEBTOBER)")
            ) // Scala.js
          )
        },
        test("MonthDay") {
          assert(stringify("").fromSExpr[MonthDay])(
            isLeft(equalTo("( is not a valid ISO-8601 format, illegal month day at index 0)"))
          ) &&
          assert(stringify("X-01-01").fromSExpr[MonthDay])(
            isLeft(equalTo("(X-01-01 is not a valid ISO-8601 format, expected '-' at index 0)"))
          ) &&
          assert(stringify("-X01-01").fromSExpr[MonthDay])(
            isLeft(equalTo("(-X01-01 is not a valid ISO-8601 format, expected '-' at index 1)"))
          ) &&
          assert(stringify("--X1-01").fromSExpr[MonthDay])(
            isLeft(equalTo("(--X1-01 is not a valid ISO-8601 format, expected digit at index 2)"))
          ) &&
          assert(stringify("--0X-01").fromSExpr[MonthDay])(
            isLeft(equalTo("(--0X-01 is not a valid ISO-8601 format, expected digit at index 3)"))
          ) &&
          assert(stringify("--00-01").fromSExpr[MonthDay])(
            isLeft(equalTo("(--00-01 is not a valid ISO-8601 format, illegal month at index 3)"))
          ) &&
          assert(stringify("--13-01").fromSExpr[MonthDay])(
            isLeft(equalTo("(--13-01 is not a valid ISO-8601 format, illegal month at index 3)"))
          ) &&
          assert(stringify("--01X01").fromSExpr[MonthDay])(
            isLeft(equalTo("(--01X01 is not a valid ISO-8601 format, expected '-' at index 4)"))
          ) &&
          assert(stringify("--01-X1").fromSExpr[MonthDay])(
            isLeft(equalTo("(--01-X1 is not a valid ISO-8601 format, expected digit at index 5)"))
          ) &&
          assert(stringify("--01-0X").fromSExpr[MonthDay])(
            isLeft(equalTo("(--01-0X is not a valid ISO-8601 format, expected digit at index 6)"))
          ) &&
          assert(stringify("--01-00").fromSExpr[MonthDay])(
            isLeft(equalTo("(--01-00 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--01-32").fromSExpr[MonthDay])(
            isLeft(equalTo("(--01-32 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--02-30").fromSExpr[MonthDay])(
            isLeft(equalTo("(--02-30 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--03-32").fromSExpr[MonthDay])(
            isLeft(equalTo("(--03-32 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--04-31").fromSExpr[MonthDay])(
            isLeft(equalTo("(--04-31 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--05-32").fromSExpr[MonthDay])(
            isLeft(equalTo("(--05-32 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--06-31").fromSExpr[MonthDay])(
            isLeft(equalTo("(--06-31 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--07-32").fromSExpr[MonthDay])(
            isLeft(equalTo("(--07-32 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--08-32").fromSExpr[MonthDay])(
            isLeft(equalTo("(--08-32 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--09-31").fromSExpr[MonthDay])(
            isLeft(equalTo("(--09-31 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--10-32").fromSExpr[MonthDay])(
            isLeft(equalTo("(--10-32 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--11-31").fromSExpr[MonthDay])(
            isLeft(equalTo("(--11-31 is not a valid ISO-8601 format, illegal day at index 6)"))
          ) &&
          assert(stringify("--12-32").fromSExpr[MonthDay])(
            isLeft(equalTo("(--12-32 is not a valid ISO-8601 format, illegal day at index 6)"))
          )
        },
        test("OffsetDateTime") {
          assert(stringify("").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("( is not a valid ISO-8601 format, illegal offset date time at index 0)")
            )
          ) &&
          assert(stringify("2020").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020 is not a valid ISO-8601 format, illegal offset date time at index 0)")
            )
          ) &&
          assert(stringify("2020-0").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-0 is not a valid ISO-8601 format, illegal offset date time at index 5)")
            )
          ) &&
          assert(stringify("2020-01-0").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-0 is not a valid ISO-8601 format, illegal offset date time at index 8)")
            )
          ) &&
          assert(stringify("2020-01-01T0").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T0 is not a valid ISO-8601 format, illegal offset date time at index 11)")
            )
          ) &&
          assert(stringify("2020-01-01T01:0").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:0 is not a valid ISO-8601 format, illegal offset date time at index 14)")
            )
          ) &&
          assert(stringify("X020-01-01T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(X020-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or '+' or digit at index 0)")
            )
          ) &&
          assert(stringify("2X20-01-01T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2X20-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("20X0-01-01T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(20X0-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("202X-01-01T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(202X-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("2020X01-01T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020X01-01T01:01Z is not a valid ISO-8601 format, expected '-' at index 4)")
            )
          ) &&
          assert(stringify("2020-X1-01T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-X1-01T01:01Z is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("2020-0X-01T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-0X-01T01:01Z is not a valid ISO-8601 format, expected digit at index 6)")
            )
          ) &&
          assert(stringify("2020-01X01T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01X01T01:01Z is not a valid ISO-8601 format, expected '-' at index 7)")
            )
          ) &&
          assert(stringify("2020-01-X1T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-X1T01:01Z is not a valid ISO-8601 format, expected digit at index 8)")
            )
          ) &&
          assert(stringify("2020-01-0XT01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-0XT01:01Z is not a valid ISO-8601 format, expected digit at index 9)")
            )
          ) &&
          assert(stringify("2020-01-01X01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01X01:01Z is not a valid ISO-8601 format, expected 'T' at index 10)")
            )
          ) &&
          assert(stringify("2020-01-01TX1:01").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01TX1:01 is not a valid ISO-8601 format, expected digit at index 11)")
            )
          ) &&
          assert(stringify("2020-01-01T0X:01").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T0X:01 is not a valid ISO-8601 format, expected digit at index 12)")
            )
          ) &&
          assert(stringify("2020-01-01T24:01").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T24:01 is not a valid ISO-8601 format, illegal hour at index 12)")
            )
          ) &&
          assert(stringify("2020-01-01T01X01").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01X01 is not a valid ISO-8601 format, expected ':' at index 13)")
            )
          ) &&
          assert(stringify("2020-01-01T01:X1").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:X1 is not a valid ISO-8601 format, expected digit at index 14)")
            )
          ) &&
          assert(stringify("2020-01-01T01:0X").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:0X is not a valid ISO-8601 format, expected digit at index 15)")
            )
          ) &&
          assert(stringify("2020-01-01T01:60").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:60 is not a valid ISO-8601 format, illegal minute at index 15)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01 is not a valid ISO-8601 format, expected ':' or '+' or '-' or 'Z' at index 16)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01X").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01X is not a valid ISO-8601 format, expected ':' or '+' or '-' or 'Z' at index 16)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:0").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:0 is not a valid ISO-8601 format, illegal offset date time at index 17)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:X1Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:X1Z is not a valid ISO-8601 format, expected digit at index 17)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:0XZ").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:0XZ is not a valid ISO-8601 format, expected digit at index 18)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:60Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:60Z is not a valid ISO-8601 format, illegal second at index 18)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01 is not a valid ISO-8601 format, expected '.' or '+' or '-' or 'Z' at index 19)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:012").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:012 is not a valid ISO-8601 format, expected '.' or '+' or '-' or 'Z' at index 19)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01.").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01. is not a valid ISO-8601 format, expected digit or '+' or '-' or 'Z' at index 20)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01.X").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01.X is not a valid ISO-8601 format, expected digit or '+' or '-' or 'Z' at index 20)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01.123456789X").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01.123456789X is not a valid ISO-8601 format, expected '+' or '-' or 'Z' at index 29)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01ZX").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01ZX is not a valid ISO-8601 format, illegal offset date time at index 20)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+X1:01:01").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+X1:01:01 is not a valid ISO-8601 format, expected digit at index 20)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+0").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+0 is not a valid ISO-8601 format, illegal offset date time at index 20)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+0X:01:01").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+0X:01:01 is not a valid ISO-8601 format, expected digit at index 21)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+19:01:01").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01+19:01:01 is not a valid ISO-8601 format, illegal timezone offset hour at index 21)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01X01:01").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01+01X01:01 is not a valid ISO-8601 format, illegal offset date time at index 23)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:0").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+01:0 is not a valid ISO-8601 format, illegal offset date time at index 23)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:X1:01").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+01:X1:01 is not a valid ISO-8601 format, expected digit at index 23)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:0X:01").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+01:0X:01 is not a valid ISO-8601 format, expected digit at index 24)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:60:01").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01+01:60:01 is not a valid ISO-8601 format, illegal timezone offset minute at index 24)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01X01").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01+01:01X01 is not a valid ISO-8601 format, illegal offset date time at index 26)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:0").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01+01:01:0 is not a valid ISO-8601 format, illegal offset date time at index 26)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:X1").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+01:01:X1 is not a valid ISO-8601 format, expected digit at index 26)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:0X").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+01:01:0X is not a valid ISO-8601 format, expected digit at index 27)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:60").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01+01:01:60 is not a valid ISO-8601 format, illegal timezone offset second at index 27)"
              )
            )
          ) &&
          assert(stringify("+X0000-01-01T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(+X0000-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("+1X000-01-01T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(+1X000-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("+10X00-01-01T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(+10X00-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("+100X0-01-01T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(+100X0-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 4)")
            )
          ) &&
          assert(stringify("+1000X-01-01T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(+1000X-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("+10000X-01-01T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(+10000X-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or digit at index 6)")
            )
          ) &&
          assert(stringify("+100000X-01-01T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(+100000X-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or digit at index 7)")
            )
          ) &&
          assert(stringify("+1000000X-01-01T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(+1000000X-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or digit at index 8)")
            )
          ) &&
          assert(stringify("+1000000000-01-01T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(+1000000000-01-01T01:01Z is not a valid ISO-8601 format, expected '-' at index 10)")
            )
          ) &&
          assert(stringify("-1000000000-01-01T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(-1000000000-01-01T01:01Z is not a valid ISO-8601 format, expected '-' at index 10)")
            )
          ) &&
          assert(stringify("-0000-01-01T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(-0000-01-01T01:01Z is not a valid ISO-8601 format, illegal year at index 4)")
            )
          ) &&
          assert(stringify("+10000").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(+10000 is not a valid ISO-8601 format, illegal offset date time at index 6)")
            )
          ) &&
          assert(stringify("2020-00-01T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-00-01T01:01Z is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-13-01T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-13-01T01:01Z is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-01-00T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-00T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-01-32T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-01-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-02-30T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-02-30T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-03-32T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-03-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-04-31T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-04-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-05-32T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-05-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-06-31T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-06-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-07-32T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-07-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-08-32T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-08-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-09-31T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-09-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-10-32T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-10-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-11-31T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-11-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-12-32T01:01Z").fromSExpr[OffsetDateTime])(
            isLeft(
              equalTo("(2020-12-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          )
        },
        test("OffsetTime") {
          assert(stringify("").fromSExpr[OffsetTime])(
            isLeft(
              equalTo("( is not a valid ISO-8601 format, illegal offset time at index 0)")
            )
          ) &&
          assert(stringify("0").fromSExpr[OffsetTime])(
            isLeft(
              equalTo("(0 is not a valid ISO-8601 format, illegal offset time at index 0)")
            )
          ) &&
          assert(stringify("01:0").fromSExpr[OffsetTime])(
            isLeft(
              equalTo("(01:0 is not a valid ISO-8601 format, illegal offset time at index 3)")
            )
          ) &&
          assert(stringify("X1:01").fromSExpr[OffsetTime])(
            isLeft(
              equalTo("(X1:01 is not a valid ISO-8601 format, expected digit at index 0)")
            )
          ) &&
          assert(stringify("0X:01").fromSExpr[OffsetTime])(
            isLeft(
              equalTo("(0X:01 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("24:01").fromSExpr[OffsetTime])(
            isLeft(
              equalTo("(24:01 is not a valid ISO-8601 format, illegal hour at index 1)")
            )
          ) &&
          assert(stringify("01X01").fromSExpr[OffsetTime])(
            isLeft(
              equalTo("(01X01 is not a valid ISO-8601 format, expected ':' at index 2)")
            )
          ) &&
          assert(stringify("01:X1").fromSExpr[OffsetTime])(
            isLeft(
              equalTo("(01:X1 is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("01:0X").fromSExpr[OffsetTime])(
            isLeft(
              equalTo("(01:0X is not a valid ISO-8601 format, expected digit at index 4)")
            )
          ) &&
          assert(stringify("01:60").fromSExpr[OffsetTime])(
            isLeft(
              equalTo("(01:60 is not a valid ISO-8601 format, illegal minute at index 4)")
            )
          ) &&
          assert(stringify("01:01").fromSExpr[OffsetTime])(
            isLeft(
              equalTo(
                "(01:01 is not a valid ISO-8601 format, expected ':' or '+' or '-' or 'Z' at index 5)"
              )
            )
          ) &&
          assert(stringify("01:01X").fromSExpr[OffsetTime])(
            isLeft(
              equalTo(
                "(01:01X is not a valid ISO-8601 format, expected ':' or '+' or '-' or 'Z' at index 5)"
              )
            )
          ) &&
          assert(stringify("01:01:0").fromSExpr[OffsetTime])(
            isLeft(
              equalTo("(01:01:0 is not a valid ISO-8601 format, illegal offset time at index 6)")
            )
          ) &&
          assert(stringify("01:01:X1Z").fromSExpr[OffsetTime])(
            isLeft(
              equalTo("(01:01:X1Z is not a valid ISO-8601 format, expected digit at index 6)")
            )
          ) &&
          assert(stringify("01:01:0XZ").fromSExpr[OffsetTime])(
            isLeft(
              equalTo("(01:01:0XZ is not a valid ISO-8601 format, expected digit at index 7)")
            )
          ) &&
          assert(stringify("01:01:60Z").fromSExpr[OffsetTime])(
            isLeft(
              equalTo("(01:01:60Z is not a valid ISO-8601 format, illegal second at index 7)")
            )
          ) &&
          assert(stringify("01:01:01").fromSExpr[OffsetTime])(
            isLeft(
              equalTo(
                "(01:01:01 is not a valid ISO-8601 format, expected '.' or '+' or '-' or 'Z' at index 8)"
              )
            )
          ) &&
          assert(stringify("01:01:012").fromSExpr[OffsetTime])(
            isLeft(
              equalTo(
                "(01:01:012 is not a valid ISO-8601 format, expected '.' or '+' or '-' or 'Z' at index 8)"
              )
            )
          ) &&
          assert(stringify("01:01:01.").fromSExpr[OffsetTime])(
            isLeft(
              equalTo(
                "(01:01:01. is not a valid ISO-8601 format, expected digit or '+' or '-' or 'Z' at index 9)"
              )
            )
          ) &&
          assert(stringify("01:01:01.X").fromSExpr[OffsetTime])(
            isLeft(
              equalTo(
                "(01:01:01.X is not a valid ISO-8601 format, expected digit or '+' or '-' or 'Z' at index 9)"
              )
            )
          ) &&
          assert(stringify("01:01:01.123456789X").fromSExpr[OffsetTime])(
            isLeft(
              equalTo(
                "(01:01:01.123456789X is not a valid ISO-8601 format, expected '+' or '-' or 'Z' at index 18)"
              )
            )
          ) &&
          assert(stringify("01:01:01ZX").fromSExpr[OffsetTime])(
            isLeft(
              equalTo("(01:01:01ZX is not a valid ISO-8601 format, illegal offset time at index 9)")
            )
          ) &&
          assert(stringify("01:01:01+X1:01:01").fromSExpr[OffsetTime])(
            isLeft(
              equalTo("(01:01:01+X1:01:01 is not a valid ISO-8601 format, expected digit at index 9)")
            )
          ) &&
          assert(stringify("01:01:01+0").fromSExpr[OffsetTime])(
            isLeft(
              equalTo("(01:01:01+0 is not a valid ISO-8601 format, illegal offset time at index 9)")
            )
          ) &&
          assert(stringify("01:01:01+0X:01:01").fromSExpr[OffsetTime])(
            isLeft(
              equalTo("(01:01:01+0X:01:01 is not a valid ISO-8601 format, expected digit at index 10)")
            )
          ) &&
          assert(stringify("01:01:01+19:01:01").fromSExpr[OffsetTime])(
            isLeft(
              equalTo("(01:01:01+19:01:01 is not a valid ISO-8601 format, illegal timezone offset hour at index 10)")
            )
          ) &&
          assert(stringify("01:01:01+01X01:01").fromSExpr[OffsetTime])(
            isLeft(
              equalTo("(01:01:01+01X01:01 is not a valid ISO-8601 format, illegal offset time at index 12)")
            )
          ) &&
          assert(stringify("01:01:01+01:0").fromSExpr[OffsetTime])(
            isLeft(
              equalTo("(01:01:01+01:0 is not a valid ISO-8601 format, illegal offset time at index 12)")
            )
          ) &&
          assert(stringify("01:01:01+01:X1:01").fromSExpr[OffsetTime])(
            isLeft(
              equalTo("(01:01:01+01:X1:01 is not a valid ISO-8601 format, expected digit at index 12)")
            )
          ) &&
          assert(stringify("01:01:01+01:0X:01").fromSExpr[OffsetTime])(
            isLeft(
              equalTo("(01:01:01+01:0X:01 is not a valid ISO-8601 format, expected digit at index 13)")
            )
          ) &&
          assert(stringify("01:01:01+01:60:01").fromSExpr[OffsetTime])(
            isLeft(
              equalTo("(01:01:01+01:60:01 is not a valid ISO-8601 format, illegal timezone offset minute at index 13)")
            )
          ) &&
          assert(stringify("01:01:01+01:01X01").fromSExpr[OffsetTime])(
            isLeft(
              equalTo("(01:01:01+01:01X01 is not a valid ISO-8601 format, illegal offset time at index 15)")
            )
          ) &&
          assert(stringify("01:01:01+01:01:0").fromSExpr[OffsetTime])(
            isLeft(
              equalTo("(01:01:01+01:01:0 is not a valid ISO-8601 format, illegal offset time at index 15)")
            )
          ) &&
          assert(stringify("01:01:01+01:01:X1").fromSExpr[OffsetTime])(
            isLeft(
              equalTo("(01:01:01+01:01:X1 is not a valid ISO-8601 format, expected digit at index 15)")
            )
          ) &&
          assert(stringify("01:01:01+01:01:0X").fromSExpr[OffsetTime])(
            isLeft(
              equalTo("(01:01:01+01:01:0X is not a valid ISO-8601 format, expected digit at index 16)")
            )
          ) &&
          assert(stringify("01:01:01+01:01:60").fromSExpr[OffsetTime])(
            isLeft(
              equalTo("(01:01:01+01:01:60 is not a valid ISO-8601 format, illegal timezone offset second at index 16)")
            )
          )
        },
        test("Period") {
          assert(stringify("").fromSExpr[Period])(
            isLeft(equalTo("( is not a valid ISO-8601 format, illegal period at index 0)"))
          ) &&
          assert(stringify("X").fromSExpr[Period])(
            isLeft(equalTo("(X is not a valid ISO-8601 format, expected 'P' or '-' at index 0)"))
          ) &&
          assert(stringify("P").fromSExpr[Period])(
            isLeft(equalTo("(P is not a valid ISO-8601 format, illegal period at index 1)"))
          ) &&
          assert(stringify("-").fromSExpr[Period])(
            isLeft(equalTo("(- is not a valid ISO-8601 format, illegal period at index 1)"))
          ) &&
          assert(stringify("PXY").fromSExpr[Period])(
            isLeft(equalTo("(PXY is not a valid ISO-8601 format, expected '-' or digit at index 1)"))
          ) &&
          assert(stringify("P-").fromSExpr[Period])(
            isLeft(equalTo("(P- is not a valid ISO-8601 format, illegal period at index 2)"))
          ) &&
          assert(stringify("P-XY").fromSExpr[Period])(
            isLeft(equalTo("(P-XY is not a valid ISO-8601 format, expected digit at index 2)"))
          ) &&
          assert(stringify("P1XY").fromSExpr[Period])(
            isLeft(
              equalTo("(P1XY is not a valid ISO-8601 format, expected 'Y' or 'M' or 'W' or 'D' or digit at index 2)")
            )
          ) &&
          assert(stringify("P2147483648Y").fromSExpr[Period])(
            isLeft(equalTo("(P2147483648Y is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P21474836470Y").fromSExpr[Period])(
            isLeft(equalTo("(P21474836470Y is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P-2147483649Y").fromSExpr[Period])(
            isLeft(equalTo("(P-2147483649Y is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P2147483648M").fromSExpr[Period])(
            isLeft(equalTo("(P2147483648M is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P21474836470M").fromSExpr[Period])(
            isLeft(equalTo("(P21474836470M is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P-2147483649M").fromSExpr[Period])(
            isLeft(equalTo("(P-2147483649M is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P2147483648W").fromSExpr[Period])(
            isLeft(equalTo("(P2147483648W is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P21474836470W").fromSExpr[Period])(
            isLeft(equalTo("(P21474836470W is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P-2147483649W").fromSExpr[Period])(
            isLeft(equalTo("(P-2147483649W is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P2147483648D").fromSExpr[Period])(
            isLeft(equalTo("(P2147483648D is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P21474836470D").fromSExpr[Period])(
            isLeft(equalTo("(P21474836470D is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P-2147483649D").fromSExpr[Period])(
            isLeft(equalTo("(P-2147483649D is not a valid ISO-8601 format, illegal period at index 11)"))
          ) &&
          assert(stringify("P1YXM").fromSExpr[Period])(
            isLeft(equalTo("(P1YXM is not a valid ISO-8601 format, expected '-' or digit at index 3)"))
          ) &&
          assert(stringify("P1Y-XM").fromSExpr[Period])(
            isLeft(equalTo("(P1Y-XM is not a valid ISO-8601 format, expected digit at index 4)"))
          ) &&
          assert(stringify("P1Y1XM").fromSExpr[Period])(
            isLeft(equalTo("(P1Y1XM is not a valid ISO-8601 format, expected 'M' or 'W' or 'D' or digit at index 4)"))
          ) &&
          assert(stringify("P1Y2147483648M").fromSExpr[Period])(
            isLeft(equalTo("(P1Y2147483648M is not a valid ISO-8601 format, illegal period at index 13)"))
          ) &&
          assert(stringify("P1Y21474836470M").fromSExpr[Period])(
            isLeft(equalTo("(P1Y21474836470M is not a valid ISO-8601 format, illegal period at index 13)"))
          ) &&
          assert(stringify("P1Y-2147483649M").fromSExpr[Period])(
            isLeft(equalTo("(P1Y-2147483649M is not a valid ISO-8601 format, illegal period at index 13)"))
          ) &&
          assert(stringify("P1Y2147483648W").fromSExpr[Period])(
            isLeft(equalTo("(P1Y2147483648W is not a valid ISO-8601 format, illegal period at index 13)"))
          ) &&
          assert(stringify("P1Y21474836470W").fromSExpr[Period])(
            isLeft(equalTo("(P1Y21474836470W is not a valid ISO-8601 format, illegal period at index 13)"))
          ) &&
          assert(stringify("P1Y-2147483649W").fromSExpr[Period])(
            isLeft(equalTo("(P1Y-2147483649W is not a valid ISO-8601 format, illegal period at index 13)"))
          ) &&
          assert(stringify("P1Y2147483648D").fromSExpr[Period])(
            isLeft(equalTo("(P1Y2147483648D is not a valid ISO-8601 format, illegal period at index 13)"))
          ) &&
          assert(stringify("P1Y21474836470D").fromSExpr[Period])(
            isLeft(equalTo("(P1Y21474836470D is not a valid ISO-8601 format, illegal period at index 13)"))
          ) &&
          assert(stringify("P1Y-2147483649D").fromSExpr[Period])(
            isLeft(equalTo("(P1Y-2147483649D is not a valid ISO-8601 format, illegal period at index 13)"))
          ) &&
          assert(stringify("P1Y1MXW").fromSExpr[Period])(
            isLeft(equalTo("(P1Y1MXW is not a valid ISO-8601 format, expected '\"' or '-' or digit at index 5)"))
          ) &&
          assert(stringify("P1Y1M-XW").fromSExpr[Period])(
            isLeft(equalTo("(P1Y1M-XW is not a valid ISO-8601 format, expected digit at index 6)"))
          ) &&
          assert(stringify("P1Y1M1XW").fromSExpr[Period])(
            isLeft(equalTo("(P1Y1M1XW is not a valid ISO-8601 format, expected 'W' or 'D' or digit at index 6)"))
          ) &&
          assert(stringify("P1Y1M306783379W").fromSExpr[Period])(
            isLeft(equalTo("(P1Y1M306783379W is not a valid ISO-8601 format, illegal period at index 14)"))
          ) &&
          assert(stringify("P1Y1M3067833790W").fromSExpr[Period])(
            isLeft(equalTo("(P1Y1M3067833790W is not a valid ISO-8601 format, illegal period at index 14)"))
          ) &&
          assert(stringify("P1Y1M-306783379W").fromSExpr[Period])(
            isLeft(equalTo("(P1Y1M-306783379W is not a valid ISO-8601 format, illegal period at index 15)"))
          ) &&
          assert(stringify("P1Y1M2147483648D").fromSExpr[Period])(
            isLeft(equalTo("(P1Y1M2147483648D is not a valid ISO-8601 format, illegal period at index 15)"))
          ) &&
          assert(stringify("P1Y1M21474836470D").fromSExpr[Period])(
            isLeft(equalTo("(P1Y1M21474836470D is not a valid ISO-8601 format, illegal period at index 15)"))
          ) &&
          assert(stringify("P1Y1M-2147483649D").fromSExpr[Period])(
            isLeft(equalTo("(P1Y1M-2147483649D is not a valid ISO-8601 format, illegal period at index 15)"))
          ) &&
          assert(stringify("P1Y1M1WXD").fromSExpr[Period])(
            isLeft(equalTo("(P1Y1M1WXD is not a valid ISO-8601 format, expected '\"' or '-' or digit at index 7)"))
          ) &&
          assert(stringify("P1Y1M1W-XD").fromSExpr[Period])(
            isLeft(equalTo("(P1Y1M1W-XD is not a valid ISO-8601 format, expected digit at index 8)"))
          ) &&
          assert(stringify("P1Y1M1W1XD").fromSExpr[Period])(
            isLeft(equalTo("(P1Y1M1W1XD is not a valid ISO-8601 format, expected 'D' or digit at index 8)"))
          ) &&
          assert(stringify("P1Y1M306783378W8D").fromSExpr[Period])(
            isLeft(equalTo("(P1Y1M306783378W8D is not a valid ISO-8601 format, illegal period at index 16)"))
          ) &&
          assert(stringify("P1Y1M-306783378W-8D").fromSExpr[Period])(
            isLeft(equalTo("(P1Y1M-306783378W-8D is not a valid ISO-8601 format, illegal period at index 18)"))
          ) &&
          assert(stringify("P1Y1M1W2147483647D").fromSExpr[Period])(
            isLeft(equalTo("(P1Y1M1W2147483647D is not a valid ISO-8601 format, illegal period at index 17)"))
          ) &&
          assert(stringify("P1Y1M-1W-2147483648D").fromSExpr[Period])(
            isLeft(equalTo("(P1Y1M-1W-2147483648D is not a valid ISO-8601 format, illegal period at index 19)"))
          ) &&
          assert(stringify("P1Y1M0W2147483648D").fromSExpr[Period])(
            isLeft(equalTo("(P1Y1M0W2147483648D is not a valid ISO-8601 format, illegal period at index 17)"))
          ) &&
          assert(stringify("P1Y1M0W21474836470D").fromSExpr[Period])(
            isLeft(equalTo("(P1Y1M0W21474836470D is not a valid ISO-8601 format, illegal period at index 17)"))
          ) &&
          assert(stringify("P1Y1M0W-2147483649D").fromSExpr[Period])(
            isLeft(equalTo("(P1Y1M0W-2147483649D is not a valid ISO-8601 format, illegal period at index 17)"))
          ) &&
          assert(stringify("P1Y1M1W1DX").fromSExpr[Period])(
            isLeft(equalTo("(P1Y1M1W1DX is not a valid ISO-8601 format, illegal period at index 9)"))
          )
        },
        test("Year") {
          assert(stringify("").fromSExpr[Year])(
            isLeft(
              equalTo("( is not a valid ISO-8601 format, illegal year at index 0)")
            )
          ) &&
          assert(stringify("2").fromSExpr[Year])(
            isLeft(
              equalTo("(2 is not a valid ISO-8601 format, illegal year at index 0)")
            )
          ) &&
          assert(stringify("22").fromSExpr[Year])(
            isLeft(
              equalTo("(22 is not a valid ISO-8601 format, illegal year at index 0)")
            )
          ) &&
          assert(stringify("222").fromSExpr[Year])(
            isLeft(
              equalTo("(222 is not a valid ISO-8601 format, illegal year at index 0)")
            )
          ) &&
          assert(stringify("X020").fromSExpr[Year])(
            isLeft(
              equalTo("(X020 is not a valid ISO-8601 format, expected '-' or '+' or digit at index 0)")
            )
          ) &&
          assert(stringify("2X20").fromSExpr[Year])(
            isLeft(
              equalTo("(2X20 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("20X0").fromSExpr[Year])(
            isLeft(
              equalTo("(20X0 is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("202X").fromSExpr[Year])(
            isLeft(
              equalTo("(202X is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("+X0000").fromSExpr[Year])(
            isLeft(
              equalTo("(+X0000 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("+1X000").fromSExpr[Year])(
            isLeft(
              equalTo("(+1X000 is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("+10X00").fromSExpr[Year])(
            isLeft(
              equalTo("(+10X00 is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("+100X0").fromSExpr[Year])(
            isLeft(
              equalTo("(+100X0 is not a valid ISO-8601 format, expected digit at index 4)")
            )
          ) &&
          assert(stringify("+1000X").fromSExpr[Year])(
            isLeft(
              equalTo("(+1000X is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("+10000X").fromSExpr[Year])(
            isLeft(
              equalTo("(+10000X is not a valid ISO-8601 format, expected digit at index 6)")
            )
          ) &&
          assert(stringify("+100000X").fromSExpr[Year])(
            isLeft(
              equalTo("(+100000X is not a valid ISO-8601 format, expected digit at index 7)")
            )
          ) &&
          assert(stringify("+1000000X").fromSExpr[Year])(
            isLeft(
              equalTo("(+1000000X is not a valid ISO-8601 format, expected digit at index 8)")
            )
          ) &&
          assert(stringify("+1000000000").fromSExpr[Year])(
            isLeft(
              equalTo("(+1000000000 is not a valid ISO-8601 format, illegal year at index 10)")
            )
          ) &&
          assert(stringify("-1000000000").fromSExpr[Year])(
            isLeft(
              equalTo("(-1000000000 is not a valid ISO-8601 format, illegal year at index 10)")
            )
          ) &&
          assert(stringify("-0000").fromSExpr[Year])(
            isLeft(
              equalTo("(-0000 is not a valid ISO-8601 format, illegal year at index 4)")
            )
          ) &&
          assert(stringify("10000").fromSExpr[Year])(
            isLeft(
              equalTo("(10000 is not a valid ISO-8601 format, illegal year at index 4)")
            )
          )
        },
        test("YearMonth") {
          assert(stringify("").fromSExpr[YearMonth])(
            isLeft(
              equalTo("( is not a valid ISO-8601 format, illegal year month at index 0)")
            )
          ) &&
          assert(stringify("2020").fromSExpr[YearMonth])(
            isLeft(
              equalTo("(2020 is not a valid ISO-8601 format, illegal year month at index 0)")
            )
          ) &&
          assert(stringify("2020-0").fromSExpr[YearMonth])(
            isLeft(
              equalTo("(2020-0 is not a valid ISO-8601 format, illegal year month at index 5)")
            )
          ) &&
          assert(stringify("2020-012").fromSExpr[YearMonth])(
            isLeft(
              equalTo("(2020-012 is not a valid ISO-8601 format, illegal year month at index 7)")
            )
          ) &&
          assert(stringify("X020-01").fromSExpr[YearMonth])(
            isLeft(
              equalTo("(X020-01 is not a valid ISO-8601 format, expected '-' or '+' or digit at index 0)")
            )
          ) &&
          assert(stringify("2X20-01").fromSExpr[YearMonth])(
            isLeft(
              equalTo("(2X20-01 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("20X0-01").fromSExpr[YearMonth])(
            isLeft(
              equalTo("(20X0-01 is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("202X-01").fromSExpr[YearMonth])(
            isLeft(
              equalTo("(202X-01 is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("2020X01").fromSExpr[YearMonth])(
            isLeft(
              equalTo("(2020X01 is not a valid ISO-8601 format, expected '-' at index 4)")
            )
          ) &&
          assert(stringify("2020-X1").fromSExpr[YearMonth])(
            isLeft(
              equalTo("(2020-X1 is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("2020-0X").fromSExpr[YearMonth])(
            isLeft(
              equalTo("(2020-0X is not a valid ISO-8601 format, expected digit at index 6)")
            )
          ) &&
          assert(stringify("+X0000-01").fromSExpr[YearMonth])(
            isLeft(
              equalTo("(+X0000-01 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("+1X000-01").fromSExpr[YearMonth])(
            isLeft(
              equalTo("(+1X000-01 is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("+10X00-01").fromSExpr[YearMonth])(
            isLeft(
              equalTo("(+10X00-01 is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("+100X0-01").fromSExpr[YearMonth])(
            isLeft(
              equalTo("(+100X0-01 is not a valid ISO-8601 format, expected digit at index 4)")
            )
          ) &&
          assert(stringify("+1000X-01").fromSExpr[YearMonth])(
            isLeft(
              equalTo("(+1000X-01 is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("+10000X-01").fromSExpr[YearMonth])(
            isLeft(
              equalTo("(+10000X-01 is not a valid ISO-8601 format, expected '-' or digit at index 6)")
            )
          ) &&
          assert(stringify("+100000X-01").fromSExpr[YearMonth])(
            isLeft(
              equalTo("(+100000X-01 is not a valid ISO-8601 format, expected '-' or digit at index 7)")
            )
          ) &&
          assert(stringify("+1000000X-01").fromSExpr[YearMonth])(
            isLeft(
              equalTo("(+1000000X-01 is not a valid ISO-8601 format, expected '-' or digit at index 8)")
            )
          ) &&
          assert(stringify("+1000000000-01").fromSExpr[YearMonth])(
            isLeft(
              equalTo("(+1000000000-01 is not a valid ISO-8601 format, expected '-' at index 10)")
            )
          ) &&
          assert(stringify("-1000000000-01").fromSExpr[YearMonth])(
            isLeft(
              equalTo("(-1000000000-01 is not a valid ISO-8601 format, expected '-' at index 10)")
            )
          ) &&
          assert(stringify("-0000-01").fromSExpr[YearMonth])(
            isLeft(
              equalTo("(-0000-01 is not a valid ISO-8601 format, illegal year at index 4)")
            )
          ) &&
          assert(stringify("+10000").fromSExpr[YearMonth])(
            isLeft(
              equalTo("(+10000 is not a valid ISO-8601 format, illegal year month at index 6)")
            )
          ) &&
          assert(stringify("2020-00").fromSExpr[YearMonth])(
            isLeft(
              equalTo("(2020-00 is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-13").fromSExpr[YearMonth])(
            isLeft(
              equalTo("(2020-13 is not a valid ISO-8601 format, illegal month at index 6)")
            )
          )
        },
        test("ZonedDateTime") {
          assert(stringify("").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("( is not a valid ISO-8601 format, illegal zoned date time at index 0)")
            )
          ) &&
          assert(stringify("2020").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020 is not a valid ISO-8601 format, illegal zoned date time at index 0)")
            )
          ) &&
          assert(stringify("2020-0").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-0 is not a valid ISO-8601 format, illegal zoned date time at index 5)")
            )
          ) &&
          assert(stringify("2020-01-0").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-0 is not a valid ISO-8601 format, illegal zoned date time at index 8)")
            )
          ) &&
          assert(stringify("2020-01-01T0").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T0 is not a valid ISO-8601 format, illegal zoned date time at index 11)")
            )
          ) &&
          assert(stringify("2020-01-01T01:0").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:0 is not a valid ISO-8601 format, illegal zoned date time at index 14)")
            )
          ) &&
          assert(stringify("X020-01-01T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(X020-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or '+' or digit at index 0)")
            )
          ) &&
          assert(stringify("2X20-01-01T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2X20-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("20X0-01-01T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(20X0-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("202X-01-01T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(202X-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("2020X01-01T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020X01-01T01:01Z is not a valid ISO-8601 format, expected '-' at index 4)")
            )
          ) &&
          assert(stringify("2020-X1-01T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-X1-01T01:01Z is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("2020-0X-01T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-0X-01T01:01Z is not a valid ISO-8601 format, expected digit at index 6)")
            )
          ) &&
          assert(stringify("2020-01X01T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01X01T01:01Z is not a valid ISO-8601 format, expected '-' at index 7)")
            )
          ) &&
          assert(stringify("2020-01-X1T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-X1T01:01Z is not a valid ISO-8601 format, expected digit at index 8)")
            )
          ) &&
          assert(stringify("2020-01-0XT01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-0XT01:01Z is not a valid ISO-8601 format, expected digit at index 9)")
            )
          ) &&
          assert(stringify("2020-01-01X01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01X01:01Z is not a valid ISO-8601 format, expected 'T' at index 10)")
            )
          ) &&
          assert(stringify("2020-01-01TX1:01").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01TX1:01 is not a valid ISO-8601 format, expected digit at index 11)")
            )
          ) &&
          assert(stringify("2020-01-01T0X:01").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T0X:01 is not a valid ISO-8601 format, expected digit at index 12)")
            )
          ) &&
          assert(stringify("2020-01-01T24:01").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T24:01 is not a valid ISO-8601 format, illegal hour at index 12)")
            )
          ) &&
          assert(stringify("2020-01-01T01X01").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01X01 is not a valid ISO-8601 format, expected ':' at index 13)")
            )
          ) &&
          assert(stringify("2020-01-01T01:X1").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:X1 is not a valid ISO-8601 format, expected digit at index 14)")
            )
          ) &&
          assert(stringify("2020-01-01T01:0X").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:0X is not a valid ISO-8601 format, expected digit at index 15)")
            )
          ) &&
          assert(stringify("2020-01-01T01:60").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:60 is not a valid ISO-8601 format, illegal minute at index 15)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01 is not a valid ISO-8601 format, expected ':' or '+' or '-' or 'Z' at index 16)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01X").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01X is not a valid ISO-8601 format, expected ':' or '+' or '-' or 'Z' at index 16)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:0").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:0 is not a valid ISO-8601 format, illegal zoned date time at index 17)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:X1Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:X1Z is not a valid ISO-8601 format, expected digit at index 17)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:0XZ").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:0XZ is not a valid ISO-8601 format, expected digit at index 18)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:60Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:60Z is not a valid ISO-8601 format, illegal second at index 18)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01 is not a valid ISO-8601 format, expected '.' or '+' or '-' or 'Z' at index 19)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:012").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:012 is not a valid ISO-8601 format, expected '.' or '+' or '-' or 'Z' at index 19)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01.").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01. is not a valid ISO-8601 format, expected digit or '+' or '-' or 'Z' at index 20)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01.X").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01.X is not a valid ISO-8601 format, expected digit or '+' or '-' or 'Z' at index 20)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01.123456789X").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01.123456789X is not a valid ISO-8601 format, expected '+' or '-' or 'Z' at index 29)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01ZX").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01ZX is not a valid ISO-8601 format, expected '[' at index 20)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+X1:01:01").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+X1:01:01 is not a valid ISO-8601 format, expected digit at index 20)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+0").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+0 is not a valid ISO-8601 format, illegal zoned date time at index 20)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+0X:01:01").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+0X:01:01 is not a valid ISO-8601 format, expected digit at index 21)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+19:01:01").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01+19:01:01 is not a valid ISO-8601 format, illegal timezone offset hour at index 21)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01X01:01").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01+01X01:01 is not a valid ISO-8601 format, expected '[' at index 22)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:0").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+01:0 is not a valid ISO-8601 format, illegal zoned date time at index 23)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:X1:01").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+01:X1:01 is not a valid ISO-8601 format, expected digit at index 23)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:0X:01").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+01:0X:01 is not a valid ISO-8601 format, expected digit at index 24)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:60:01").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01+01:60:01 is not a valid ISO-8601 format, illegal timezone offset minute at index 24)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01X01").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01+01:01X01 is not a valid ISO-8601 format, expected '[' at index 25)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:0").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01+01:01:0 is not a valid ISO-8601 format, illegal zoned date time at index 26)"
              )
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:X1").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+01:01:X1 is not a valid ISO-8601 format, expected digit at index 26)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:0X").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+01:01:0X is not a valid ISO-8601 format, expected digit at index 27)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:01X").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01:01+01:01:01X is not a valid ISO-8601 format, expected '[' at index 28)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01:01+01:01:60").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo(
                "(2020-01-01T01:01:01+01:01:60 is not a valid ISO-8601 format, illegal timezone offset second at index 27)"
              )
            )
          ) &&
          assert(stringify("+X0000-01-01T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(+X0000-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("+1X000-01-01T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(+1X000-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("+10X00-01-01T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(+10X00-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 3)")
            )
          ) &&
          assert(stringify("+100X0-01-01T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(+100X0-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 4)")
            )
          ) &&
          assert(stringify("+1000X-01-01T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(+1000X-01-01T01:01Z is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("+10000X-01-01T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(+10000X-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or digit at index 6)")
            )
          ) &&
          assert(stringify("+100000X-01-01T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(+100000X-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or digit at index 7)")
            )
          ) &&
          assert(stringify("+1000000X-01-01T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(+1000000X-01-01T01:01Z is not a valid ISO-8601 format, expected '-' or digit at index 8)")
            )
          ) &&
          assert(stringify("+1000000000-01-01T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(+1000000000-01-01T01:01Z is not a valid ISO-8601 format, expected '-' at index 10)")
            )
          ) &&
          assert(stringify("-1000000000-01-01T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(-1000000000-01-01T01:01Z is not a valid ISO-8601 format, expected '-' at index 10)")
            )
          ) &&
          assert(stringify("-0000-01-01T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(-0000-01-01T01:01Z is not a valid ISO-8601 format, illegal year at index 4)")
            )
          ) &&
          assert(stringify("+10000").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(+10000 is not a valid ISO-8601 format, illegal zoned date time at index 6)")
            )
          ) &&
          assert(stringify("2020-00-01T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-00-01T01:01Z is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-13-01T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-13-01T01:01Z is not a valid ISO-8601 format, illegal month at index 6)")
            )
          ) &&
          assert(stringify("2020-01-00T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-00T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-01-32T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-02-30T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-02-30T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-03-32T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-03-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-04-31T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-04-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-05-32T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-05-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-06-31T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-06-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-07-32T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-07-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-08-32T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-08-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-09-31T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-09-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-10-32T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-10-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-11-31T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-11-31T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-12-32T01:01Z").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-12-32T01:01Z is not a valid ISO-8601 format, illegal day at index 9)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01Z[").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01Z[ is not a valid ISO-8601 format, illegal zoned date time at index 17)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01Z[X]").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01Z[X] is not a valid ISO-8601 format, illegal zoned date time at index 18)")
            )
          ) &&
          assert(stringify("2020-01-01T01:01Z[GMT]X").fromSExpr[ZonedDateTime])(
            isLeft(
              equalTo("(2020-01-01T01:01Z[GMT]X is not a valid ISO-8601 format, illegal zoned date time at index 22)")
            )
          )
        },
        test("ZoneId") {
          assert(stringify("America/New York").fromSExpr[ZoneId])(
            isLeft(equalTo("(America/New York is not a valid ISO-8601 format, illegal zone id at index 0)"))
          ) &&
          assert(stringify("Solar_System/Mars").fromSExpr[ZoneId])(
            isLeft(equalTo("(Solar_System/Mars is not a valid ISO-8601 format, illegal zone id at index 0)"))
          )
        },
        test("ZoneOffset") {
          assert(stringify("").fromSExpr[ZoneOffset])(
            isLeft(
              equalTo("( is not a valid ISO-8601 format, illegal zone offset at index 0)")
            )
          ) &&
          assert(stringify("X").fromSExpr[ZoneOffset])(
            isLeft(
              equalTo("(X is not a valid ISO-8601 format, expected '+' or '-' or 'Z' at index 0)")
            )
          ) &&
          assert(stringify("+X1:01:01").fromSExpr[ZoneOffset])(
            isLeft(
              equalTo("(+X1:01:01 is not a valid ISO-8601 format, expected digit at index 1)")
            )
          ) &&
          assert(stringify("+0").fromSExpr[ZoneOffset])(
            isLeft(
              equalTo("(+0 is not a valid ISO-8601 format, illegal zone offset at index 1)")
            )
          ) &&
          assert(stringify("+0X:01:01").fromSExpr[ZoneOffset])(
            isLeft(
              equalTo("(+0X:01:01 is not a valid ISO-8601 format, expected digit at index 2)")
            )
          ) &&
          assert(stringify("+19:01:01").fromSExpr[ZoneOffset])(
            isLeft(
              equalTo(
                "(+19:01:01 is not a valid ISO-8601 format, illegal timezone offset hour at index 2)"
              )
            )
          ) &&
          assert(stringify("+01X01:01").fromSExpr[ZoneOffset])(
            isLeft(
              equalTo(
                "(+01X01:01 is not a valid ISO-8601 format, illegal zone offset at index 4)"
              )
            )
          ) &&
          assert(stringify("+01:0").fromSExpr[ZoneOffset])(
            isLeft(
              equalTo("(+01:0 is not a valid ISO-8601 format, illegal zone offset at index 4)")
            )
          ) &&
          assert(stringify("+01:X1:01").fromSExpr[ZoneOffset])(
            isLeft(
              equalTo("(+01:X1:01 is not a valid ISO-8601 format, expected digit at index 4)")
            )
          ) &&
          assert(stringify("+01:0X:01").fromSExpr[ZoneOffset])(
            isLeft(
              equalTo("(+01:0X:01 is not a valid ISO-8601 format, expected digit at index 5)")
            )
          ) &&
          assert(stringify("+01:60:01").fromSExpr[ZoneOffset])(
            isLeft(
              equalTo(
                "(+01:60:01 is not a valid ISO-8601 format, illegal timezone offset minute at index 5)"
              )
            )
          ) &&
          assert(stringify("+01:01X01").fromSExpr[ZoneOffset])(
            isLeft(
              equalTo(
                "(+01:01X01 is not a valid ISO-8601 format, illegal zone offset at index 7)"
              )
            )
          ) &&
          assert(stringify("+01:01:0").fromSExpr[ZoneOffset])(
            isLeft(
              equalTo(
                "(+01:01:0 is not a valid ISO-8601 format, illegal zone offset at index 7)"
              )
            )
          ) &&
          assert(stringify("+01:01:X1").fromSExpr[ZoneOffset])(
            isLeft(
              equalTo("(+01:01:X1 is not a valid ISO-8601 format, expected digit at index 7)")
            )
          ) &&
          assert(stringify("+01:01:0X").fromSExpr[ZoneOffset])(
            isLeft(
              equalTo("(+01:01:0X is not a valid ISO-8601 format, expected digit at index 8)")
            )
          ) &&
          assert(stringify("+01:01:60").fromSExpr[ZoneOffset])(
            isLeft(
              equalTo(
                "(+01:01:60 is not a valid ISO-8601 format, illegal timezone offset second at index 8)"
              )
            )
          )
        }
      )
    )
}
