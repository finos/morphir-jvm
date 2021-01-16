/*
Copyright 2020 Morgan Stanley

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

package morphir.sdk

import zio.test.Assertion._
import zio.test._

object BasicsSpec extends DefaultRunnableSpec {
  def spec = suite("BasicsSpec")(
    suite("Basics.Float spec")(
      testM("Construct from Float") {
        check(Gen.anyFloat) { (f: Float) =>
          val instance = Basics.Float(f)
          assert(instance)(equalTo(f.doubleValue()))
        }
      },
      testM("Construct from Double") {
        check(Gen.anyDouble) { (d: Double) =>
          val instance = Basics.Float(d)
          assert(instance)(equalTo(d))
        }
      },
      testM("Construct from Int") {
        check(Gen.anyInt) { (i: Int) =>
          val instance = Basics.Float(i)
          assert(instance)(equalTo(i.doubleValue()))
        }
      }
    ),
    suite("Basics.add spec")(
      testM("Add a Float value to another Float value") {
        check(Gen.anyDouble, Gen.anyDouble) { (d1: Basics.Float, d2: Basics.Float) =>
          val expected = d1 + d2
          assert(Basics.add(d1)(d2))(equalTo(expected))
        }
      }
    ),
    suite("Basics.subtract spec")(
      testM("Subtract a Float value from another Float value") {
        check(Gen.anyDouble, Gen.anyDouble) { (d1: Basics.Float, d2: Basics.Float) =>
          val expected = d1 - d2
          assert(Basics.subtract(d1)(d2))(equalTo(expected))
        }
      }
    ),
    suite("Basics.multiply spec")(
      testM("Multiply a Float value by another Float value") {
        check(Gen.anyDouble, Gen.anyDouble) { (d1: Basics.Float, d2: Basics.Float) =>
          val expected = d1 * d2
          assert(Basics.multiply(d1)(d2))(equalTo(expected))
        }
      }
    ),
    suite("Basics.divide spec")(
      testM("Divide a Float value by another Float value") {
        check(Gen.anyDouble, Gen.anyDouble.filter(n => n != 0)) { (d1: Basics.Float, d2: Basics.Float) =>
          val expected = d1 / d2
          assert(Basics.divide(d1)(d2))(equalTo(expected))
        }
      }
    ),
    suite("Basics.power spec")(
      testM("Power an Int value by another Int value") {
        check(Gen.anyLong, Gen.long(-10, 10)) { (d1: Basics.Int, d2: Basics.Int) =>
          val expected = d1 ^ d2
          assert(Basics.power(d1)(d2))(equalTo(expected.toLong))
        }
      }
    ),
    suite("Basics.equal spec")(
      testM("Equal check a Float value to itself") {
        check(Gen.anyDouble) { (dn: Double) =>
          val expected = true
          val d        = Basics.Float(dn)
          assert(Basics.equal(d)(d))(equalTo(expected))
        }
      },
      testM("Equal check a Float value to a different Float value") {
        check(Gen.anyDouble) { (dn: Double) =>
          val expected = false
          val d1       = Basics.Float(dn)
          val d2       = Basics.Float(dn + 3.14)
          assert(Basics.equal(d1)(d2))(equalTo(expected))
        }
      }
    ),
    suite("Basics.notEqual spec")(
      testM("Not-Equal check a Float value to itself") {
        check(Gen.anyDouble) { (dn: Double) =>
          val expected = false
          val d        = Basics.Float(dn)
          assert(Basics.notEqual(d)(d))(equalTo(expected))
        }
      },
      testM("Not-Equal check a Float value to a different Float value") {
        check(Gen.anyDouble) { (dn: Double) =>
          val expected = true
          val d1       = Basics.Float(dn)
          val d2       = Basics.Float(dn + 3.14)
          assert(Basics.notEqual(d1)(d2))(equalTo(expected))
        }
      }
    ),
    suite("Basics.lessThan spec")(
      testM("Performing lessThan check on Floats") {
        check(Gen.anyDouble, Gen.anyDouble) { (d1: Basics.Float, d2: Basics.Float) =>
          val expected = (d1 < d2)
          assert(Basics.lessThan(d1)(d2))(equalTo(expected))
        }
      }
    ),
    suite("Basics.greaterThan spec")(
      testM("Performing lessThan check on Floats") {
        check(Gen.anyDouble, Gen.anyDouble) { (d1: Basics.Float, d2: Basics.Float) =>
          val expected = (d1 > d2)
          assert(Basics.greaterThan(d1)(d2))(equalTo(expected))
        }
      }
    ),
    suite("Basics.lessThanOrEqual spec")(
      testM("Performing lessThanOrEqual check on different Floats") {
        check(Gen.anyDouble, Gen.anyDouble) { (d1: Basics.Float, d2: Basics.Float) =>
          val expected = (d1 <= d2)
          assert(Basics.lessThanOrEqual(d1)(d2))(equalTo(expected))
        }
      },
      testM("Performing lessThanOrEqual check on same Float") {
        check(Gen.anyDouble) { dn: Double =>
          val d1       = Basics.Float(dn)
          val d2       = Basics.Float(dn)
          val expected = (d1 <= d2)
          assert(Basics.lessThanOrEqual(d1)(d1))(equalTo(expected))
        }
      }
    ),
    suite("Basics.greaterThanOrEqual spec")(
      testM("Performing greaterThanOrEqual check on different Floats") {
        check(Gen.anyDouble, Gen.anyDouble) { (d1: Basics.Float, d2: Basics.Float) =>
          val expected = (d1 >= d2)
          assert(Basics.greaterThanOrEqual(d1)(d2))(equalTo(expected))
        }
      },
      testM("Performing greaterThanOrEqual check on same Float") {
        check(Gen.anyDouble) { dn: Double =>
          val d1       = Basics.Float(dn)
          val d2       = Basics.Float(dn)
          val expected = (d1 >= d2)
          assert(Basics.lessThanOrEqual(d1)(d1))(equalTo(expected))
        }
      }
    ),
    suite("BoolSpec")(
      test("Bool xor - true xor true")(
        assert(Basics.xor(Basics.Bool(true))(Basics.Bool(true)))(isFalse)
      ),
      test("Bool xor - true xor false")(
        assert(Basics.xor(Basics.Bool(true))(Basics.Bool(false)))(isFalse)
      )
    )
  )
}
