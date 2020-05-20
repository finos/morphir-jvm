package org.morphir.sdk

import zio.test._
import zio.test.Assertion._
import Int._
import org.morphir.sdk

object IntSpec extends DefaultRunnableSpec {
  def spec = suite("IntSpec")(
    suite("Int.divide spec")(
      testM("Dividing an Int8 value by an Int8 value") {
        check(Gen.anyByte, Gen.anyByte.filter(n => n != 0)) { (x: Int8, y: Int8) =>
          val expected: scala.Int = x / y
          assert(sdk.Int.divide(x)(y).toInt)(equalTo(expected))
        }
      },
      testM("Dividing an Int16 value by an Int16 value") {
        check(Gen.anyShort, Gen.anyShort.filter(n => n != 0)) { (x: Int16, y: Int16) =>
          val expected: scala.Int = x / y
          assert(sdk.Int.divide(x)(y).toInt)(equalTo(expected))
        }
      },
      testM("Dividing an Int32 value by an Int32 value") {
        check(Gen.anyInt, Gen.anyInt.filter(n => n != 0)) { (x: Int32, y: Int32) =>
          val expected: scala.Int = x / y
          assert(sdk.Int.divide(x)(y))(equalTo(expected))
        }
      },
      testM("Dividing an Int64 value by an Int64 value") {
        check(Gen.anyLong, Gen.anyLong.filter(n => n != 0)) { (x: Int64, y: Int64) =>
          val expected: scala.Long = x / y
          assert(sdk.Int.divide(x)(y))(equalTo(expected))
        }
      },
      testM("Dividing an Int value by an Int value") {
        check(Gen.anyLong, Gen.anyLong.filter(n => n != 0)) { (x: Int64, y: Int64) =>
          val bigX             = BigInt(x)
          val bigY             = BigInt(y)
          val expected: BigInt = bigX / bigY
          assert(sdk.Int.divide(bigX)(bigY))(equalTo(expected))
        }
      }
    ),
    suite("Int.modBy spec")(
      testM("Performing ModBy on Int8s") {
        check(Gen.anyByte.filter(n => n != 0), Gen.anyByte) { (divisor, dividend) =>
          val expected: scala.Int = (dividend % divisor).abs
          assert(sdk.Int.modBy(divisor)(dividend).toInt)(equalTo(expected))
        }
      },
      testM("Performing ModBy on Int16s") {
        check(Gen.anyShort.filter(n => n != 0), Gen.anyShort) { (divisor, dividend) =>
          val expected: scala.Int = (dividend % divisor).abs
          assert(sdk.Int.modBy(divisor)(dividend).toInt)(equalTo(expected))
        }
      },
      testM("Performing ModBy on Int32s") {
        check(Gen.anyInt.filter(n => n != 0), Gen.anyInt) { (divisor, dividend) =>
          val expected: scala.Int = (dividend % divisor).abs
          assert(sdk.Int.modBy(divisor)(dividend))(equalTo(expected))
        }
      },
      testM("Performing ModBy on Int64s") {
        check(Gen.anyLong.filter(n => n != 0), Gen.anyLong) { (divisor, dividend) =>
          val expected: scala.Long = (dividend % divisor).abs
          assert(sdk.Int.modBy(divisor)(dividend))(equalTo(expected))
        }
      },
      testM("Performing ModBy on Ints") {
        check(Gen.anyLong.filter(n => n != 0), Gen.anyLong) { (longDivisor, longDividend) =>
          val divisor                = BigInt(longDivisor)
          val dividend               = BigInt(longDividend)
          val expected: scala.BigInt = (dividend % divisor).abs
          assert(sdk.Int.modBy(divisor)(dividend))(equalTo(expected))
        }
      }
    ),
    suite("Int.remainderBy spec")(
      testM("Performing remainderBy on Int8s") {
        check(Gen.anyByte.filter(n => n != 0), Gen.anyByte) { (divisor, dividend) =>
          val expected: scala.Int = dividend % divisor
          assert(sdk.Int.remainderBy(divisor)(dividend).toInt)(
            equalTo(expected)
          )
        }
      },
      testM("Performing remainderBy on Int16s") {
        check(Gen.anyShort.filter(n => n != 0), Gen.anyShort) { (divisor, dividend) =>
          val expected: scala.Int = dividend % divisor
          assert(sdk.Int.remainderBy(divisor)(dividend).toInt)(
            equalTo(expected)
          )
        }
      },
      testM("Performing remainderBy on Int32s") {
        check(Gen.anyInt.filter(n => n != 0), Gen.anyInt) { (divisor, dividend) =>
          val expected: scala.Int = dividend % divisor
          assert(sdk.Int.remainderBy(divisor)(dividend))(equalTo(expected))
        }
      },
      testM("Performing remainderBy on Int64s") {
        check(Gen.anyLong.filter(n => n != 0), Gen.anyLong) { (divisor, dividend) =>
          val expected: scala.Long = dividend % divisor
          assert(sdk.Int.remainderBy(divisor)(dividend))(equalTo(expected))
        }
      },
      testM("Performing remainderBy on Ints") {
        check(Gen.anyLong.filter(n => n != 0), Gen.anyLong) { (longDivisor, longDividend) =>
          val divisor                = BigInt(longDivisor)
          val dividend               = BigInt(longDividend)
          val expected: scala.BigInt = dividend % divisor
          assert(sdk.Int.remainderBy(divisor)(dividend))(equalTo(expected))
        }
      }
    )
  )
}
