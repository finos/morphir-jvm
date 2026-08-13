package morphir.sdk

import zio.test.Assertion._
import zio.test._

import morphir.testing.MorphirBaseSpec

object DecimalSpec extends MorphirBaseSpec {
  def spec = suite("Decimal Spec")(
    test("It should be possible to assign an int value to the Decimal") {
      assert(Decimal(42))(equalTo(Decimal.fromInt(42)))
    },
    suite("Decimal.toFloat")(
      test("converts a positive integer") {
        assert(Decimal.toFloat(Decimal.fromInt(1)))(equalTo(1.0))
      },
      test("converts zero") {
        assert(Decimal.toFloat(Decimal.fromInt(0)))(equalTo(0.0))
      },
      test("converts a negative integer") {
        assert(Decimal.toFloat(Decimal.fromInt(-1)))(equalTo(-1.0))
      },
      test("converts a decimal value") {
        assert(Decimal.toFloat(Decimal.fromFloat(3.14)))(approximatelyEquals(3.14, 1.0e-9))
      },
      test("roundtrips fromFloat") {
        assert(Decimal.toFloat(Decimal.fromFloat(1.5)))(approximatelyEquals(1.5, 1.0e-9))
      }
    )
  )
}
