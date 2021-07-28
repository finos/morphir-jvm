package morphir.sdk

import zio.test.Assertion._
import zio.test._

object DecimalSpec extends DefaultRunnableSpec {
  def spec = suite("Decimal Spec")(
    test("It should be possible to assign an int value to the Decimal") {
      assert(Decimal(42))(equalTo(Decimal.fromInt(42)))
    }
  )
}
