package zio.morphir.sexpr

import zio.morphir.sexpr.ast._
import zio.morphir.testing.ZioBaseSpec
import zio.test.Assertion._
import zio.test._
import zio.{Chunk, NonEmptyChunk}

import java.time.{LocalTime, _}
import java.util.UUID
import scala.collection.immutable.*
import scala.collection.{SortedMap, immutable}

object DecoderSpec extends ZioBaseSpec {
  def wrap(s: String): String = s""""$s""""

  def spec = suite("Decoder")(
    suite("fromSExpr")(
      suite("primitives")(
        test("string") {
          assertTrue(
            "\"hello world\"".fromSExpr[String] == Right("hello world"),
            "\"hello\\nworld\"".fromSExpr[String] == Right("hello\nworld"),
            "\"hello\\rworld\"".fromSExpr[String] == Right("hello\rworld"),
            "\"hello\\u0000world\"".fromSExpr[String] == Right("hello\u0000world")
          )
        },
        test("bigInt") {
          check(Gens.genBigInteger) { x =>
            assertTrue(x.toString.fromSExpr[java.math.BigInteger] == Right(x))
          }
        },
        test("bigDecimal") {
          check(Gens.genBigDecimal) { x =>
            assertTrue(x.toString.fromSExpr[java.math.BigDecimal] == Right(x))
          }
        },
        test("boolean") {
          assertTrue("true".fromSExpr[Boolean] == Right(true)) &&
          assertTrue("false".fromSExpr[Boolean] == Right(false))
        },
        test("byte") {
          check(Gen.byte) { x =>
            assertTrue(x.toString.fromSExpr[Byte] == Right(x))
          }
        },
        test("char") {
          check(Gen.char) { x =>
            assertTrue(s""""${x.toString}"""".fromSExpr[Char] == Right(x))
          }
        },
        test("double") {
          check(Gen.double) { x =>
            assertTrue(x.toString.fromSExpr[Double] == Right(x))
          }
        },
        test("float") {
          check(Gen.float) { x =>
            assertTrue(x.toString.fromSExpr[Float] == Right(x))
          }
        },
        test("int") {
          check(Gen.int) { x =>
            assertTrue(x.toString.fromSExpr[Int] == Right(x))
          }
        },
        test("long") {
          check(Gen.long) { x =>
            assertTrue(x.toString.fromSExpr[Long] == Right(x))
          }
        },
        test("short") {
          check(Gen.short) { x =>
            assertTrue(x.toString.fromSExpr[Short] == Right(x))
          }
        }
      ),
      suite("java.util.UUID")(
        test("Auto-generated") {
          check(Gen.uuid) { x =>
            assertTrue(s""""${x.toString}"""".fromSExpr[UUID] == Right(x))
          }
        }
        // test("Manual") {
        //   val ok1  = "64d7c38d-2afd-4514-9832-4e70afe4b0f8"
        //   val ok2  = "0000000064D7C38D-FD-14-32-70AFE4B0f8"
        //   val ok3  = "0-0-0-0-0"
        //   val bad1 = ""
        //   val bad2 = "64d7c38d-2afd-4514-9832-4e70afe4b0f80"
        //   val bad3 = "64d7c38d-2afd-4514-983-4e70afe4b0f80"
        //   val bad4 = "64d7c38d-2afd--9832-4e70afe4b0f8"
        //   val bad5 = "64d7c38d-2afd-XXXX-9832-4e70afe4b0f8"
        //   val bad6 = "64d7c38d-2afd-X-9832-4e70afe4b0f8"
        //   val bad7 = "0-0-0-0-00000000000000000"

        //   assertTrue(
        //     wrap(ok1).fromSExpr[UUID] == Right(UUID.fromString(ok1)),
        //     wrap(ok2).fromSExpr[UUID] == Right(UUID.fromString(ok2)),
        //     wrap(ok3).fromSExpr[UUID] == Right(UUID.fromString(ok3)),
        //     wrap(bad1).fromSExpr[UUID] == Left(s"(Invalid UUID: $bad1)"),
        //     wrap(bad2).fromSExpr[UUID] == Left(s"(Invalid UUID: UUID string too large)"),
        //     wrap(bad3).fromSExpr[UUID] == Left(s"(Invalid UUID: $bad3)"),
        //     wrap(bad4).fromSExpr[UUID] == Left(s"(Invalid UUID: $bad4)"),
        //     wrap(bad5).fromSExpr[UUID] == Left(s"(Invalid UUID: $bad5)"),
        //     wrap(bad6).fromSExpr[UUID] == Left(s"(Invalid UUID: $bad6)"),
        //     wrap(bad7).fromSExpr[UUID] == Left(s"(Invalid UUID: $bad7)")
        //   )
        // }
      ),
      suite("java.time")(
        suite("Duration")(
          test("Auto-generated") {
            check(Gen.finiteDuration) { x =>
              assertTrue(s""""${x.toString}"""".fromSExpr[Duration] == Right(x))
            }
          },
          test("Manual") {
            val ok1  = "PT1H2M3S"
            val ok2  = "PT-0.5S" // see https://bugs.java.com/bugdatabase/view_bug.do?bug_id=8054978
            val bad1 = "PT-H"

            assertTrue(
              wrap(ok1).fromSExpr[Duration] == Right(java.time.Duration.parse(ok1)),
              wrap(ok2).fromSExpr[Duration] == Right(java.time.Duration.ofNanos(-500000000)),
              wrap(bad1).fromSExpr[Duration] == Left(
                s"($bad1 is not a valid ISO-8601 format, expected digit at index 3)"
              )
            )
          }
        ),
        test("DayOfWeek") {
          check(Gen.dayOfWeek) { x =>
            assertTrue(s""""${x.toString}"""".fromSExpr[DayOfWeek] == Right(x))
          }
        },
        test("Instant") {
          check(Gen.instant) { x =>
            assertTrue(s""""${x.toString}"""".fromSExpr[Instant] == Right(x))
          }
        },
        test("LocalDate") {
          check(Gen.localDate) { x =>
            assertTrue(s""""${x.toString}"""".fromSExpr[LocalDate] == Right(x))
          }
        },
        test("LocalDateTime") {
          check(Gen.localDateTime) { x =>
            assertTrue(s""""${x.toString}"""".fromSExpr[LocalDateTime] == Right(x))
          }
        },
        test("LocalTime") {
          check(Gen.localTime) { x =>
            assertTrue(s""""${x.toString}"""".fromSExpr[LocalTime] == Right(x))
          }
        },
        test("Month") {
          check(Gen.month) { x =>
            assertTrue(s""""${x.toString}"""".fromSExpr[Month] == Right(x))
          }
        },
        test("MonthDay") {
          check(Gen.monthDay) { x =>
            assertTrue(s""""${x.toString}"""".fromSExpr[MonthDay] == Right(x))
          }
        },
        test("OffsetDateTime") {
          check(Gen.offsetDateTime) { x =>
            assertTrue(s""""${x.toString}"""".fromSExpr[OffsetDateTime] == Right(x))
          }
        },
        test("OffsetTime") {
          check(Gen.offsetTime) { x =>
            assertTrue(s""""${x.toString}"""".fromSExpr[OffsetTime] == Right(x))
          }
        },
        test("Period") {
          check(Gen.period) { x =>
            assertTrue(s""""${x.toString}"""".fromSExpr[Period] == Right(x))
          }
        },
        test("Year") {
          check(Gens.genYear) { x =>
            val year = "%04d".format(x.getValue)
            assertTrue(s""""$year"""".fromSExpr[Year] == Right(x))
          }
        },
        test("YearMonth") {
          check(Gens.genYearMonth) { x =>
            assertTrue(s""""${x.toString}"""".fromSExpr[YearMonth] == Right(x))
          }
        },
        test("ZoneId") {
          check(Gen.zoneId) { x =>
            assertTrue(s""""${x.toString}"""".fromSExpr[ZoneId] == Right(x))
          }
        },
        test("ZoneOffset") {
          check(Gen.zoneOffset) { x =>
            assertTrue(s""""${x.toString}"""".fromSExpr[ZoneOffset] == Right(x))
          }
        },
        suite("ZonedDateTime")(
          test("Auto-generated") {
            check(Gen.zonedDateTime) { x =>
              assertTrue(s""""${x.toString}"""".fromSExpr[ZonedDateTime] == Right(x))
            }
          },
          test("Manual") {
            val ok1 = "2021-06-20T20:03:51.533418+02:00[Europe/Warsaw]"
            val ok2 = "2018-10-28T02:30+00:00[Europe/Warsaw]" // see https://bugs.openjdk.java.net/browse/JDK-8066982
            val result2 = "2018-10-28T03:30+01:00[Europe/Warsaw]"
            val bad1    = "2018-10-28T02:30"

            assertTrue(
              wrap(ok1).fromSExpr[ZonedDateTime] == Right(ZonedDateTime.parse(ok1)),
              wrap(ok2).fromSExpr[ZonedDateTime] == Right(ZonedDateTime.parse(result2)),
              wrap(bad1).fromSExpr[ZonedDateTime] == Left(
                s"($bad1 is not a valid ISO-8601 format, expected ':' or '+' or '-' or 'Z' at index 16)"
              )
            )
          }
        )
      ),
      suite("Collections")(
        test("List empty") {
          val sexprStr = "[]"
          val expected = List[String]()

          assertTrue(sexprStr.fromSExpr[List[String]] == Right(expected))
        },
        test("Array") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = Array("5XL", "2XL", "XL")

          assert(sexprStr.fromSExpr[Array[String]])(isRight(equalTo(expected)))
        },
        test("List") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = List("5XL", "2XL", "XL")

          assertTrue(sexprStr.fromSExpr[List[String]] == Right(expected))
        },
        test("Seq") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = Seq("5XL", "2XL", "XL")

          assertTrue(sexprStr.fromSExpr[Seq[String]] == Right(expected))
        },
        test("IndexedSeq") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = IndexedSeq("5XL", "2XL", "XL")

          assertTrue(sexprStr.fromSExpr[IndexedSeq[String]] == Right(expected))
        },
        test("LinearSeq") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = LinearSeq("5XL", "2XL", "XL")

          assertTrue(sexprStr.fromSExpr[LinearSeq[String]] == Right(expected))
        },
        test("Vector") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = Vector("5XL", "2XL", "XL")

          assertTrue(sexprStr.fromSExpr[Vector[String]] == Right(expected))
        },
        test("Set") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = Set("5XL", "2XL", "XL")

          assertTrue(sexprStr.fromSExpr[Set[String]] == Right(expected))
        },
        test("HashSet") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = immutable.HashSet("5XL", "2XL", "XL")

          assertTrue(sexprStr.fromSExpr[immutable.HashSet[String]] == Right(expected))
        },
        test("ListSet") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = ListSet("5XL", "2XL", "XL")

          assertTrue(sexprStr.fromSExpr[ListSet[String]] == Right(expected))
        },
        test("SortedSet") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = SortedSet("5XL", "2XL", "XL")

          assertTrue(sexprStr.fromSExpr[SortedSet[String]] == Right(expected))
        },
        test("TreeSet") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = TreeSet("5XL", "2XL", "XL")

          assertTrue(sexprStr.fromSExpr[TreeSet[String]] == Right(expected))
        },
        test("zio.Chunk") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = Chunk("5XL", "2XL", "XL")

          assertTrue(sexprStr.fromSExpr[Chunk[String]] == Right(expected))
        },
        test("zio.NonEmptyChunk") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = NonEmptyChunk("5XL", "2XL", "XL")

          assertTrue(sexprStr.fromSExpr[NonEmptyChunk[String]] == Right(expected))
        },
        test("zio.NonEmptyChunk failure") {
          assertTrue("[]".fromSExpr[NonEmptyChunk[String]] == Left("(Chunk was empty)"))
        },
        test("Iterable") {
          val sexprStr = """["5XL","2XL","XL"]"""
          val expected = Iterable("5XL", "2XL", "XL")

          assertTrue(sexprStr.fromSExpr[immutable.Iterable[String]] == Right(expected))
        },
        test("Collections of Int") {
          val arr = "[\n1, \n2, \n3\n]"
          assert(arr.fromSExpr[Array[Int]])(isRight(equalTo(Array(1, 2, 3)))) &&
          assertTrue(
            arr.fromSExpr[List[Int]] == Right(List(1, 2, 3)),
            arr.fromSExpr[Seq[Int]] == Right(Seq(1, 2, 3)),
            arr.fromSExpr[IndexedSeq[Int]] == Right(IndexedSeq(1, 2, 3)),
            arr.fromSExpr[LinearSeq[Int]] == Right(LinearSeq(1, 2, 3)),
            arr.fromSExpr[Vector[Int]] == Right(Vector(1, 2, 3)),
            arr.fromSExpr[Set[Int]] == Right(Set(1, 2, 3)),
            arr.fromSExpr[HashSet[Int]] == Right(HashSet(1, 2, 3)),
            arr.fromSExpr[ListSet[Int]] == Right(ListSet(1, 2, 3)),
            arr.fromSExpr[SortedSet[Int]] == Right(SortedSet(1, 2, 3)),
            arr.fromSExpr[TreeSet[Int]] == Right(TreeSet(1, 2, 3)),
            arr.fromSExpr[Iterable[Int]] == Right(Iterable(1, 2, 3)),
            arr.fromSExpr[zio.Chunk[Int]] == Right(zio.Chunk(1, 2, 3))
          )
        },
        test("HashMap empty") {
          val sexprStr = """{}"""
          val expected = HashMap[String, Int]()

          assertTrue(sexprStr.fromSExpr[HashMap[String, Int]] == Right(expected))
        },
        test("HashMap") {
          val sexprStr = """{"5XL" 3, "2XL" 14, "XL" 159}"""
          val expected = HashMap("5XL" -> 3, "2XL" -> 14, "XL" -> 159)

          assertTrue(sexprStr.fromSExpr[HashMap[String, Int]] == Right(expected))
        },
        test("Map") {
          val sexprStr = """{"5XL" 3, "2XL" 14, "XL" 159}"""
          val expected = HashMap("5XL" -> 3, "2XL" -> 14, "XL" -> 159)

          assertTrue(sexprStr.fromSExpr[Map[String, Int]] == Right(expected))
        },
        test("SortedMap") {
          val sexprStr = """{"5XL" 3, "2XL" 14, "XL" 159}"""
          val expected = TreeMap("5XL" -> 3, "2XL" -> 14, "XL" -> 159)

          assertTrue(sexprStr.fromSExpr[SortedMap[String, Int]] == Right(expected))
        },
        test("TreeMap") {
          val sexprStr = """{"5XL" 3, "2XL" 14, "XL" 159}"""
          val expected = TreeMap("5XL" -> 3, "2XL" -> 14, "XL" -> 159)

          assertTrue(sexprStr.fromSExpr[TreeMap[String, Int]] == Right(expected))
        },
        test("mutable.Map") {
          val sexprStr = """{"5XL" 3, "2XL" 14, "XL" 159}"""
          val expected = collection.mutable.Map("5XL" -> 3, "2XL" -> 14, "XL" -> 159)

          assertTrue(sexprStr.fromSExpr[collection.mutable.Map[String, Int]] == Right(expected))
        },
        test("mutable.HashMap") {
          val sexprStr = """{"5XL" 3, "2XL" 14, "XL" 159}"""
          val expected = collection.mutable.HashMap("5XL" -> 3, "2XL" -> 14, "XL" -> 159)

          assertTrue(sexprStr.fromSExpr[collection.mutable.HashMap[String, Int]] == Right(expected))
        }
      ),
      test("Option") {
        assertTrue("nil".fromSExpr[Option[Boolean]] == Right(None)) &&
        assertTrue("false".fromSExpr[Option[Boolean]] == Right(Some(false))) &&
        assertTrue("true".fromSExpr[Option[Boolean]] == Right(Some(true))) &&
        assertTrue("nil".fromSExpr[Option[Int]] == Right(None)) &&
        assertTrue("26".fromSExpr[Option[Int]] == Right(Some(26)))
      },
      test("Either") {
        val ones = List("""{a  1}""", """{left 1 }""", """{  Left 1  }""")
        val twos = List("""{ b 2 }""", """{ right 2}""", """{Right 2}""")

        assert(ones.map(_.fromSExpr[Either[Int, Int]]))(forall(isRight(isLeft(equalTo(1))))) &&
        assert(twos.map(_.fromSExpr[Either[Int, Int]]))(forall(isRight(isRight(equalTo(2)))))
      },
      suite("SExpr")(
        test("Bool") {
          assertTrue(
            "true".fromSExpr[SExpr.Bool] == Right(SExpr.Bool.True),
            "false".fromSExpr[SExpr.Bool] == Right(SExpr.Bool.False)
          )
        },
        test("Nil") {
          assertTrue(
            "nil".fromSExpr[SExpr.Nil.type] == Right(SExpr.Nil)
          )
        },
        test("Str") {
          assertTrue(
            """"nil"""".fromSExpr[SExpr.Str] == Right(SExpr.Str("nil")),
            """"null"""".fromSExpr[SExpr.Str] == Right(SExpr.Str("null"))
            // """"\t\n\"""".fromSExpr[SExpr.Str] == Right(SExpr.Str("\t\n"))
          )
        },
        test("Num") {
          assertTrue(
            "22.22".fromSExpr[SExpr.Num] == Right(SExpr.Num(BigDecimal("22.22"))),
            "2222".fromSExpr[SExpr.Num] == Right(SExpr.Num(BigDecimal("2222"))),
            "-2222".fromSExpr[SExpr.Num] == Right(SExpr.Num(BigDecimal("-2222"))),
            "-22.22".fromSExpr[SExpr.Num] == Right(SExpr.Num(BigDecimal("-22.22")))
          )
        },
        test("Symbol") {
          assertTrue(
            "symb1".fromSExpr[SExpr.Symbol] == Right(SExpr.Symbol("symb1", SymbolKind.Standard)),
            "null".fromSExpr[SExpr.Symbol] == Right(SExpr.Symbol("null", SymbolKind.Standard)),
            "#symb".fromSExpr[SExpr.Symbol] == Right(SExpr.Symbol("#symb", SymbolKind.Standard)),
            "_ns/il".fromSExpr[SExpr.Symbol] == Right(SExpr.Symbol("_ns/il", SymbolKind.Standard)),
            ".n/il".fromSExpr[SExpr.Symbol] == Right(SExpr.Symbol(".n/il", SymbolKind.Standard)),
            ":keyWord6".fromSExpr[SExpr.Symbol] == Right(SExpr.Symbol(":keyWord6", SymbolKind.Keyword)),
            "::keywrdMacro".fromSExpr[SExpr.Symbol] == Right(SExpr.Symbol("::keywrdMacro", SymbolKind.Macro))
          )
        },
        test("SVector") {
          assertTrue(
            """[true -1 nil "Hello" null 3.14159 false]""".fromSExpr[SExpr.SVector] == Right(
              SExpr.SVector(
                Chunk(
                  SExpr.Bool.True,
                  SExpr.Num(BigDecimal(-1)),
                  SExpr.Nil,
                  SExpr.Str("Hello"),
                  SExpr.Symbol("null"),
                  SExpr.Num(BigDecimal(3.14159)),
                  SExpr.Bool.False
                )
              )
            )
          )
        },
        test("SMap") {
          val map = """{ "x" 0, :keyword1 "hello", ::#vect [1 2.1 -0.3333], nil true }"""
          assertTrue(
            map.fromSExpr[SExpr.SMap[SExpr, SExpr]] ==
              Right(
                SExpr.SMap[SExpr, SExpr](
                  Chunk(
                    SExpr.Str("x")                                -> SExpr.Num(BigDecimal(0)),
                    SExpr.Symbol(":keyword1", SymbolKind.Keyword) -> SExpr.Str("hello"),
                    SExpr.Symbol("::#vect", SymbolKind.Macro) -> SExpr.SVector(
                      Chunk(
                        SExpr.Num(BigDecimal(1)),
                        SExpr.Num(BigDecimal(2.1)),
                        SExpr.Num(BigDecimal(-0.3333))
                      )
                    ),
                    SExpr.Nil -> SExpr.Bool.True
                  ).toMap
                )
              )
          )
        }
      )
    ),
    suite("fromAST")(
      suite("primitives")(
        test("string") {
          assertTrue(
            """"hello world"""".fromSExpr[String] == Right("hello world")
            // """"hello\\nworld"""".fromSExpr[String] == Right("hello\nworld"),
            // """"hello\\rworld"""""".fromSExpr[String] == Right("hello\rworld"),
            // """"hello\\u0000world"""".fromSExpr[String] == Right("hello\u0000world")
          )
        },
        test("bigInt") {
          check(Gens.genBigInteger) { x =>
            assertTrue(SExpr.Num(x).as[java.math.BigInteger] == Right(x))
          }
        },
        test("bigDecimal") {
          check(Gens.genBigDecimal) { x =>
            assertTrue(SExpr.Num(x).as[java.math.BigDecimal] == Right(x))
          }
        },
        test("boolean") {
          assertTrue(
            SExpr.Bool(true).as[Boolean] == Right(true),
            SExpr.Bool(false).as[Boolean] == Right(false)
          )
        },
        test("byte") {
          check(Gen.byte) { x =>
            assertTrue(SExpr.Num(x).as[Byte] == Right(x))
          }
        },
        test("char") {
          check(Gen.char) { x =>
            assertTrue(SExpr.Str(x.toString).as[Char] == Right(x))
          }
        },
        test("double") {
          check(Gen.double) { x =>
            assertTrue(SExpr.Num(x).as[Double] == Right(x))
          }
        },
        test("float") {
          check(Gen.float) { x =>
            assertTrue(SExpr.Num(x).as[Float] == Right(x))
          }
        },
        test("int") {
          check(Gen.int) { x =>
            assertTrue(SExpr.Num(x).as[Int] == Right(x))
          }
        },
        test("long") {
          check(Gen.long) { x =>
            assertTrue(SExpr.Num(x).as[Long] == Right(x))
          }
        },
        test("short") {
          check(Gen.short) { x =>
            assertTrue(SExpr.Num(x).as[Short] == Right(x))
          }
        }
      ),
      suite("java.util.UUID")(
        test("Auto-generated") {
          check(Gen.uuid) { x =>
            assertTrue(SExpr.Str(x.toString).as[UUID] == Right(x))
          }
        }
        //   test("Manual") {
        //     val ok1  = "64d7c38d-2afd-4514-9832-4e70afe4b0f8"
        //     val ok2  = "0000000064D7C38D-FD-14-32-70AFE4B0f8"
        //     val ok3  = "0-0-0-0-0"
        //     val bad1 = ""
        //     val bad2 = "64d7c38d-2afd-4514-9832-4e70afe4b0f80"
        //     val bad3 = "64d7c38d-2afd-4514-983-4e70afe4b0f80"
        //     val bad4 = "64d7c38d-2afd--9832-4e70afe4b0f8"
        //     val bad5 = "64d7c38d-2afd-XXXX-9832-4e70afe4b0f8"
        //     val bad6 = "64d7c38d-2afd-X-9832-4e70afe4b0f8"
        //     val bad7 = "0-0-0-0-00000000000000000"

        //     assertTrue(
        //       SExpr.Str(ok1).as[UUID] == Right(UUID.fromString(ok1)),
        //       SExpr.Str(ok2).as[UUID] == Right(UUID.fromString(ok2)),
        //       SExpr.Str(ok3).as[UUID] == Right(UUID.fromString(ok3)),
        //       SExpr.Str(bad1).as[UUID] == Left(s"Invalid UUID: $bad1"),
        //       SExpr.Str(bad2).as[UUID] == Left(s"Invalid UUID: UUID string too large"),
        //       SExpr.Str(bad3).as[UUID] == Left(s"Invalid UUID: $bad3"),
        //       SExpr.Str(bad4).as[UUID] == Left(s"Invalid UUID: $bad4"),
        //       SExpr.Str(bad5).as[UUID] == Left(s"Invalid UUID: $bad5"),
        //       SExpr.Str(bad6).as[UUID] == Left(s"Invalid UUID: $bad6"),
        //       SExpr.Str(bad7).as[UUID] == Left(s"Invalid UUID: $bad7")
        //     )
        //   }
      ),
      suite("java.time")(
        suite("Duration")(
          test("Auto-generated") {
            check(Gen.finiteDuration) { x =>
              assertTrue(SExpr.Str(x.toString).as[Duration] == Right(x))
            }
          },
          test("Manual") {
            val ok1  = "PT1H2M3S"
            val ok2  = "PT-0.5S" // see https://bugs.java.com/bugdatabase/view_bug.do?bug_id=8054978
            val bad1 = "PT-H"

            assertTrue(
              SExpr.Str(ok1).as[Duration] == Right(java.time.Duration.parse(ok1)),
              SExpr.Str(ok2).as[Duration] == Right(java.time.Duration.ofNanos(-500000000)),
              SExpr.Str(bad1).as[Duration] == Left("PT-H is not a valid ISO-8601 format, expected digit at index 3")
            )
          }
        ),
        test("DayOfWeek") {
          check(Gen.dayOfWeek) { x =>
            assertTrue(SExpr.Str(x.toString).as[DayOfWeek] == Right(x))
          }
        },
        test("Instant") {
          check(Gen.instant) { x =>
            assertTrue(SExpr.Str(x.toString).as[Instant] == Right(x))
          }
        },
        test("LocalDate") {
          check(Gen.localDate) { x =>
            assertTrue(SExpr.Str(x.toString).as[LocalDate] == Right(x))
          }
        },
        test("LocalDateTime") {
          check(Gen.localDateTime) { x =>
            assertTrue(SExpr.Str(x.toString).as[LocalDateTime] == Right(x))
          }
        },
        test("LocalTime") {
          check(Gen.localTime) { x =>
            assertTrue(SExpr.Str(x.toString).as[LocalTime] == Right(x))
          }
        },
        test("Month") {
          check(Gen.month) { x =>
            assertTrue(SExpr.Str(x.toString).as[Month] == Right(x))
          }
        },
        test("MonthDay") {
          check(Gen.monthDay) { x =>
            assertTrue(SExpr.Str(x.toString).as[MonthDay] == Right(x))
          }
        },
        test("OffsetDateTime") {
          check(Gen.offsetDateTime) { x =>
            assertTrue(SExpr.Str(x.toString).as[OffsetDateTime] == Right(x))
          }
        },
        test("OffsetTime") {
          check(Gen.offsetTime) { x =>
            assertTrue(SExpr.Str(x.toString).as[OffsetTime] == Right(x))
          }
        },
        test("Period") {
          check(Gen.period) { x =>
            assertTrue(SExpr.Str(x.toString).as[Period] == Right(x))
          }
        },
        test("Year") {
          check(Gens.genYear) { x =>
            assertTrue(SExpr.Str("%04d".format(x.getValue)).as[Year] == Right(x))
          }
        },
        test("YearMonth") {
          check(Gens.genYearMonth) { x =>
            assertTrue(SExpr.Str(x.toString).as[YearMonth] == Right(x))
          }
        },
        test("ZoneId") {
          check(Gen.zoneId) { x =>
            assertTrue(SExpr.Str(x.toString).as[ZoneId] == Right(x))
          }
        },
        test("ZoneOffset") {
          check(Gen.zoneOffset) { x =>
            assertTrue(SExpr.Str(x.toString).as[ZoneOffset] == Right(x))
          }
        },
        suite("ZonedDateTime")(
          test("Auto-generated") {
            check(Gen.zonedDateTime) { x =>
              assertTrue(SExpr.Str(x.toString).as[ZonedDateTime] == Right(x))
            }
          },
          test("Manual") {
            val ok1 = "2021-06-20T20:03:51.533418+02:00[Europe/Warsaw]"
            val ok2 = "2018-10-28T02:30+00:00[Europe/Warsaw]" // see https://bugs.openjdk.java.net/browse/JDK-8066982
            val result2 = "2018-10-28T03:30+01:00[Europe/Warsaw]"
            val bad1    = "2018-10-28T02:30"

            assertTrue(
              SExpr.Str(ok1.toString).as[ZonedDateTime] == Right(ZonedDateTime.parse(ok1)),
              SExpr.Str(ok2.toString).as[ZonedDateTime] == Right(ZonedDateTime.parse(result2)),
              SExpr.Str(bad1.toString).as[ZonedDateTime] == Left(
                s"$bad1 is not a valid ISO-8601 format, expected ':' or '+' or '-' or 'Z' at index 16"
              )
            )
          }
        )
      ),
      suite("Collections")(
        test("Collections of Int") {
          val sexpr = SExpr.vector(SExpr.Num(1), SExpr.Num(2), SExpr.Num(3), SExpr.Num(3))
          assertTrue(
            sexpr.as[List[Int]] == Right(List(1, 2, 3, 3)),
            sexpr.as[Seq[Int]] == Right(Seq(1, 2, 3, 3)),
            sexpr.as[IndexedSeq[Int]] == Right(IndexedSeq(1, 2, 3, 3)),
            sexpr.as[LinearSeq[Int]] == Right(LinearSeq(1, 2, 3, 3)),
            sexpr.as[Vector[Int]] == Right(Vector(1, 2, 3, 3)),
            sexpr.as[Set[Int]] == Right(Set(1, 2, 3)),
            sexpr.as[HashSet[Int]] == Right(HashSet(1, 2, 3)),
            sexpr.as[ListSet[Int]] == Right(ListSet(1, 2, 3)),
            sexpr.as[SortedSet[Int]] == Right(SortedSet(1, 2, 3)),
            sexpr.as[TreeSet[Int]] == Right(TreeSet(1, 2, 3)),
            sexpr.as[Iterable[Int]] == Right(Iterable(1, 2, 3, 3)),
            sexpr.as[zio.Chunk[Int]] == Right(zio.Chunk(1, 2, 3, 3))
          )
        },
        test("Array") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"), SExpr.Str("2XL"))
          val expected = Array("5XL", "2XL", "XL", "2XL")

          assert(sexpr.as[Array[String]])(isRight(equalTo(expected)))
        },
        test("List") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"), SExpr.Str("2XL"))
          val expected = List("5XL", "2XL", "XL", "2XL")

          assertTrue(sexpr.as[List[String]] == Right(expected))
        },
        test("Seq") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"), SExpr.Str("2XL"))
          val expected = Seq("5XL", "2XL", "XL", "2XL")

          assertTrue(sexpr.as[Seq[String]] == Right(expected))
        },
        test("IndexedSeq") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"), SExpr.Str("2XL"))
          val expected = IndexedSeq("5XL", "2XL", "XL", "2XL")

          assertTrue(sexpr.as[IndexedSeq[String]] == Right(expected))
        },
        test("LinearSeq") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"), SExpr.Str("2XL"))
          val expected = LinearSeq("5XL", "2XL", "XL", "2XL")

          assertTrue(sexpr.as[LinearSeq[String]] == Right(expected))
        },
        test("Vector") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"), SExpr.Str("2XL"))
          val expected = Vector("5XL", "2XL", "XL", "2XL")

          assertTrue(sexpr.as[Vector[String]] == Right(expected))
        },
        test("Set") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"), SExpr.Str("2XL"))
          val expected = Set("5XL", "2XL", "XL")
          assertTrue(sexpr.as[Set[String]] == Right(expected))
        },
        test("HashSet") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"), SExpr.Str("2XL"))
          val expected = immutable.HashSet("5XL", "2XL", "XL")

          assertTrue(sexpr.as[immutable.HashSet[String]] == Right(expected))
        },
        test("ListSet") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"), SExpr.Str("2XL"))
          val expected = immutable.ListSet("5XL", "2XL", "XL")

          assertTrue(sexpr.as[immutable.ListSet[String]] == Right(expected))
        },
        test("SortedSet") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"), SExpr.Str("2XL"))
          val expected = immutable.SortedSet("5XL", "2XL", "XL")

          assertTrue(sexpr.as[immutable.SortedSet[String]] == Right(expected))
        },
        test("TreeSet") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"), SExpr.Str("2XL"))
          val expected = immutable.TreeSet("5XL", "2XL", "XL")

          assertTrue(sexpr.as[immutable.TreeSet[String]] == Right(expected))
        },
        test("Iterable") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"), SExpr.Str("2XL"))
          val expected = Iterable("5XL", "2XL", "XL", "2XL")

          assertTrue(sexpr.as[Iterable[String]] == Right(expected))
        },
        test("zio.Chunk") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"), SExpr.Str("2XL"))
          val expected = zio.Chunk("5XL", "2XL", "XL", "2XL")

          assertTrue(sexpr.as[zio.Chunk[String]] == Right(expected))
        },
        test("zio.NonEmptyChunk") {
          val sexpr    = SExpr.vector(SExpr.Str("5XL"), SExpr.Str("2XL"), SExpr.Str("XL"))
          val expected = NonEmptyChunk("5XL", "2XL", "XL")

          assertTrue(sexpr.as[NonEmptyChunk[String]] == Right(expected))
        },
        test("Map") {
          val sExpr = SExpr.SMap(
            Map(
              SExpr.Str("5XL") -> SExpr.Num(new java.math.BigDecimal(3)),
              SExpr.Str("2XL") -> SExpr.Num(new java.math.BigDecimal(14)),
              SExpr.Str("XL")  -> SExpr.Num(new java.math.BigDecimal(159))
            )
          )
          val expected = Map("5XL" -> 3, "2XL" -> 14, "XL" -> 159)

          assertTrue(sExpr.as[Map[String, Int]] == Right(expected))
        },
        test("SortedMap") {
          val sExpr = SExpr.SMap(
            Map(
              SExpr.Str("5XL") -> SExpr.Num(new java.math.BigDecimal(3)),
              SExpr.Str("2XL") -> SExpr.Num(new java.math.BigDecimal(14)),
              SExpr.Str("XL")  -> SExpr.Num(new java.math.BigDecimal(159))
            )
          )
          val expected = SortedMap("5XL" -> 3, "2XL" -> 14, "XL" -> 159)

          assertTrue(sExpr.as[SortedMap[String, Int]] == Right(expected))
        },
        test("TreeMap") {
          val sExpr = SExpr.SMap(
            Map(
              SExpr.Str("5XL") -> SExpr.Num(new java.math.BigDecimal(3)),
              SExpr.Str("2XL") -> SExpr.Num(new java.math.BigDecimal(14)),
              SExpr.Str("XL")  -> SExpr.Num(new java.math.BigDecimal(159))
            )
          )
          val expected = TreeMap("5XL" -> 3, "2XL" -> 14, "XL" -> 159)

          assertTrue(sExpr.as[TreeMap[String, Int]] == Right(expected))
        },
        test("HashMap") {
          val sExpr = SExpr.SMap(
            Map(
              SExpr.Str("5XL") -> SExpr.Num(new java.math.BigDecimal(3)),
              SExpr.Str("2XL") -> SExpr.Num(new java.math.BigDecimal(14)),
              SExpr.Str("XL")  -> SExpr.Num(new java.math.BigDecimal(159))
            )
          )
          val expected = HashMap("5XL" -> 3, "2XL" -> 14, "XL" -> 159)

          assertTrue(sExpr.as[HashMap[String, Int]] == Right(expected))
        },
        test("mutable.HashMap") {
          val sExpr = SExpr.SMap(
            Map(
              SExpr.Str("5XL") -> SExpr.Num(new java.math.BigDecimal(3)),
              SExpr.Str("2XL") -> SExpr.Num(new java.math.BigDecimal(14)),
              SExpr.Str("XL")  -> SExpr.Num(new java.math.BigDecimal(159))
            )
          )
          val expected = collection.mutable.HashMap("5XL" -> 3, "2XL" -> 14, "XL" -> 159)

          assertTrue(sExpr.as[collection.mutable.HashMap[String, Int]] == Right(expected))
        },
        test("mutable.Map") {
          val sExpr = SExpr.SMap(
            Map(
              SExpr.Str("5XL") -> SExpr.Num(new java.math.BigDecimal(3)),
              SExpr.Str("2XL") -> SExpr.Num(new java.math.BigDecimal(14)),
              SExpr.Str("XL")  -> SExpr.Num(new java.math.BigDecimal(159))
            )
          )
          val expected = collection.mutable.Map("5XL" -> 3, "2XL" -> 14, "XL" -> 159)

          assertTrue(sExpr.as[collection.mutable.Map[String, Int]] == Right(expected))
        },
        test("Map, custom keys") {
          val sExpr    = SExpr.SMap(Map(SExpr.Str("1") -> SExpr.Str("a"), SExpr.Str("2") -> SExpr.Str("b")))
          val expected = Map("1" -> "a", "2" -> "b")

          assertTrue(sExpr.as[Map[String, String]] == Right(expected))
        }
      )
    ),
    test("Option") {
      assertTrue(
        SExpr.Nil.as[Option[Boolean]] == Right(None),
        SExpr.Nil.as[Option[String]] == Right(None),
        SExpr.Bool.False.as[Option[Boolean]] == Right(Some(false)),
        SExpr.Bool.True.as[Option[Boolean]] == Right(Some(true)),
        SExpr.Num(26.26).as[Option[Double]] == Right(Some(26.26)),
        SExpr.Str("hello").as[Option[String]] == Right(Some("hello"))
      )
    },
    test("Either") {
      val sexpr1 = SExpr.SMap(Chunk(SExpr.Symbol("Left", SymbolKind.Standard) -> SExpr.Str("hello1")).toMap)
      val sexpr2 = SExpr.SMap(Chunk(SExpr.Symbol("Right", SymbolKind.Standard) -> SExpr.Str("hello2")).toMap)
      val sexpr3 = SExpr.SMap(Chunk(SExpr.Symbol("left", SymbolKind.Standard) -> SExpr.Str("hello3")).toMap)
      val sexpr4 = SExpr.SMap(Chunk(SExpr.Symbol("right", SymbolKind.Standard) -> SExpr.Str("hello4")).toMap)
      val sexpr5 = SExpr.SMap(Chunk(SExpr.Symbol("a", SymbolKind.Standard) -> SExpr.Str("hello5")).toMap)
      val sexpr6 = SExpr.SMap(Chunk(SExpr.Symbol("b", SymbolKind.Standard) -> SExpr.Str("hello6")).toMap)
      val sexpr7 = SExpr.SMap(Chunk(SExpr.Str("Left") -> SExpr.Str("error case")).toMap)

      assertTrue(
        sexpr1.as[Either[String, String]] == Right(Left("hello1")),
        sexpr2.as[Either[String, String]] == Right(Right("hello2")),
        sexpr3.as[Either[String, String]] == Right(Left("hello3")),
        sexpr4.as[Either[String, String]] == Right(Right("hello4")),
        sexpr5.as[Either[String, String]] == Right(Left("hello5")),
        sexpr6.as[Either[String, String]] == Right(Right("hello6")),
        sexpr7.as[Either[String, String]] == Left("Not an either")
      )
    }
  )
}
