package zio.morphir.sexpr

import zio.morphir.sexpr.Gens._
import zio.morphir.testing.ZioBaseSpec
import zio.test._
import zio.test.Assertion._
import zio.test.TestAspect._

import java.util.UUID

object RoundTripSpec extends ZioBaseSpec {
  def spec = suite("RoundTrip")(
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
      check(Gen.uuid)(assertRoundtrips[UUID])
    } @@ samples(1000),
    test("Option") {
      check(Gen.option(Gen.int))(assertRoundtrips[Option[Int]])
    } @@ samples(1000)
  )

  private def assertRoundtrips[A: SExprEncoder: SExprDecoder](a: A) =
    assert(a.toSExpr.fromSExpr[A])(isRight(equalTo(a))) &&
      assert(a.toSExprPretty.fromSExpr[A])(isRight(equalTo(a)))
}
