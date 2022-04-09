package zio.morphir.sexpr

import zio.Random
import zio.morphir.sexpr.Gens._
import zio.morphir.testing.ZioBaseSpec
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test.{Gen, Sized, _}

import java.time._

object RoundTripSpec extends ZioBaseSpec {
  def spec: ZSpec[Environment, Failure] = suite("RoundTrip")(
    suite("primitives")(
      test("bigInt") {
        check(genBigInteger)(assertRoundtrips[java.math.BigInteger])
      } @@ samples(1000),
      test("bigDecimal") {
        check(genBigDecimal)(assertRoundtrips[java.math.BigDecimal])
      } @@ samples(1000),
      test("boolean") {
        check(Gen.boolean)(assertRoundtrips[Boolean])
      } @@ samples(1000),
      test("byte") {
        check(Gen.byte)(assertRoundtrips[Byte])
      } @@ samples(1000),
      test("char") {
        check(Gen.char)(assertRoundtrips[Char])
      } @@ samples(1000),
      test("double") {
        check(Gen.double)(assertRoundtrips[Double])
      } @@ samples(1000),
      test("float") {
        check(Gen.float)(assertRoundtrips[Float])
      } @@ samples(1000),
      test("int") {
        check(Gen.int)(assertRoundtrips[Int])
      } @@ samples(1000),
      test("long") {
        check(Gen.long)(assertRoundtrips[Long])
      } @@ samples(1000),
      test("short") {
        check(Gen.short)(assertRoundtrips[Short])
      } @@ samples(1000),
      test("string") {
        check(Gen.string)(assertRoundtrips[String])
      } @@ samples(1000)
    ),
    test("UUID") {
      check(Gen.uuid)(assertRoundtrips[java.util.UUID])
    } @@ samples(1000),
    test("Option") {
      check(Gen.option(Gen.int))(assertRoundtrips[Option[Int]])
    } @@ samples(1000),
    test("Either") {
      check(Gen.either(Gen.int, Gen.string))(assertRoundtrips[Either[Int, String]])
    } @@ samples(1000),
    test("List[Int]") {
      check(Gen.listOf(Gen.int))(assertRoundtrips[List[Int]])
    } @@ samples(1000),
    test("Vector[Double]") {
      check(Gen.vectorOf(Gen.double))(assertRoundtrips[Vector[Double]])
    } @@ samples(1000),
    test("Vector[LocalDate]") {
      check(Gen.vectorOf(Gen.localDate))(assertRoundtrips[Vector[LocalDate]])
    } @@ samples(1000),
    test("Chunk[UUID]") {
      check(Gen.chunkOf(Gen.uuid))(assertRoundtrips[zio.Chunk[java.util.UUID]])
    } @@ samples(1000),
    test("Map[Long, Int]") {
      check(Gen.mapOf(Gen.long, Gen.int))(assertRoundtrips[Map[Long, Int]])
    } @@ samples(1000),
    test("Map[List[Int], Vector[Double]]") {
      check(Gen.mapOf(Gen.listOf(Gen.int), Gen.vectorOf(Gen.double)))(assertRoundtrips[Map[List[Int], Vector[Double]]])
    } @@ samples(1000)
  )

  // TODO: Be more complete, cover more cases
  val symbolGen: Gen[Random with Sized, Symbol] = Gen
    .oneOf(
      Gen.alphaNumericString.filter(_.nonEmpty),
      Gen.alphaNumericString.map(str => "#" + str),
      Gen.alphaNumericString.map(str => "##" + str),
      Gen.const("."),
      Gen.const("/"),
      Gen.const("*")
    )
    .map(Symbol.apply)

  private def assertRoundtrips[A: SExprEncoder: SExprDecoder](a: A) =
    assert(a.toSExpr.fromSExpr[A])(isRight(equalTo(a))) &&
      assert(a.toSExprPretty.fromSExpr[A])(isRight(equalTo(a)))
}
