package test.model

import zio.{test => _, _}
import zio.test._

object AlgoSpec extends ZIOSpecDefault {
  def spec = suite("AlgoSpec")(
    test("Algo.circlePerimeter") {
      assertTrue(Algo.circlePerimeter(1.0f) == 2 * 3.14f * 1.0f)
    },
    test("Algo.complex") {
      assertTrue(Algo.complex(1, 2, 3, 4) == 1 + 2 - 3 * 4)
    },
    test("Algo.complex2") {
      assertTrue(Algo.complex2(1, 2, 3, 4) == 1 - (2 + 3) * 4)
    },
    test("Algo.divide") {
      assertTrue(Algo.divide(10, 2) == 10 / 2)
    },
    test("Algo.divideFloat") {
      assertTrue(Algo.divideFloat(10.0f, 2.0f) == 10.0f / 2.0f)
    },
    test("Algo.identity") {
      assertTrue(Algo.identity(42) == 42)
    },
    test("Algo.minus") {
      assertTrue(Algo.minus(10, 2) == 10 - 2)
    },
    test("Algo.multiply") {
      assertTrue(Algo.multiply(10, 2) == 10 * 2)
    },
    test("Algo.plus") {
      assertTrue(Algo.plus(1, 2, 3) == 1 + 2 + 3)
    }
  )
}