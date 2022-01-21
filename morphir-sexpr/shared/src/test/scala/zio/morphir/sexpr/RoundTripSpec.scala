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
    test("UUID") {
      check(Gen.uuid)(assertRoundtrips)
    } @@ samples(1000),
    test("Option") {
      check(Gen.option(Gen.int))(assertRoundtrips)
    } @@ samples(1000)
  )

  private def assertRoundtrips[A: SExprEncoder: SExprDecoder](a: A) =
    assert(a.toSExpr.fromSExpr[A])(isRight(equalTo(a))) &&
      assert(a.toSExprPretty.fromSExpr[A])(isRight(equalTo(a)))
}
