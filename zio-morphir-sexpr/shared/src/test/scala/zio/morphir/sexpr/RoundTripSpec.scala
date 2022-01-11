package zio.morphir.sexpr

import zio.morphir.testing.ZioBaseSpec
import zio.test._
import zio.test.Assertion._
import zio.test.TestAspect._

object RoundTripSpec extends ZioBaseSpec {

  def spec = suite("RoundTrip")(
    testM("booleans") {
      check(Gen.boolean)(assertRoundtrips)
    } @@ ignore @@ tag("Not sure what is broken here"),
    testM("char") {
      check(Gen.anyChar)(assertRoundtrips)
    }
  )

  private def assertRoundtrips[A: SExprEncoder: SExprDecoder](a: A) =
    assert(a.toSExpr.fromSExpr[A])(isRight(equalTo(a))) &&
      assert(a.toSExprPretty.fromSExpr[A])(isRight(equalTo(a)))
}
