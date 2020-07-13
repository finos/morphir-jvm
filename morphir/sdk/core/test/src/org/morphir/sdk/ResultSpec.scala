package org.morphir.sdk

import zio.test._
import zio.test.Assertion._

object ResultSpec extends DefaultRunnableSpec {
  def spec = suite("ResultSpec")(
    suite("Mapping")(
      suite("Calling map")(
        testM("Given an Ok value should invoke the mapping") {
          check(Gen.alphaNumericString) { input =>
            assert(
              result.map((text: String) => text.toUpperCase())(result.Ok(input))
            )(
              equalTo(result.Ok(input.toUpperCase()))
            )
          }
        },
        test("Given an Err value should return that value") {
          val original: result[String, Int] = result.Err("No Bueno!")
          assert(result.map((x: Int) => x * 2)(original))(
            equalTo(result.Err("No Bueno!"))
          )
        }
      )
    ),
    suite("Map2")(
      suite("Calling map2")(
        testM("Given an Ok value should invoke the map2") {
          check(Gen.int(1, 100), Gen.int(1, 100)) { (inputA, inputB) =>
            val inputa = result.Ok(inputA).withErr[String]
            val inputb = result.Ok(inputB).withErr[String]
            assert(
              result.map2((a: Int, b: Int) => a + b)(inputa)(inputb)
            )(
              equalTo(result.Ok(inputA + inputB))
            )
          }
        },
        test("Given an Err in value A should return that value") {
          val bad: result[String, Int] = result.Err("No Bueno!")
          val inputb                   = result.Ok(1).withErr[String]
          assert(
            result.map2((a: Int, b: Int) => a * b * 2)(bad)(inputb)
          )(
            equalTo(result.Err("No Bueno!"))
          )
        },
        test("Given an Err in value B should return that value") {
          val bad: result[String, Int] = result.Err("No Bueno!")
          val inputa                   = result.Ok(1).withErr[String]
          assert(
            result.map2((a: Int, b: Int) => a * b * 2)(inputa)(bad)
          )(
            equalTo(result.Err("No Bueno!"))
          )
        }
      )
    ),
    suite("Calling flatMap")(
      testM("Given an Ok value, then it should invoke the mapping function") {
        check(Gen.alphaNumericString, Gen.int(1, 200)) { (product, quantity) =>
          val orderItem = OrderItem(product, quantity)
          val input     = result.Ok(orderItem).withErr[String]
          assert(
            input.flatMap((oi: OrderItem) => result.Ok(Product(oi.product)).withErr[String])
          )(
            equalTo(
              result.Ok(Product(product)).withErr[String]
            )
          )
        }
      },
      test("Given an Err value, then it should return the original error") {
        val result: result[String, Unit] = result.Err("Whamo!")
        assert(result.flatMap(_ => result.Ok(42)))(
          equalTo(result.Err("Whamo!").withOk[Int])
        )
      }
    ),
    suite("Calling andThen")(
      testM("Given an Ok value, then it should invoke the mapping function") {
        check(Gen.alphaNumericString, Gen.int(1, 200)) { (product, quantity) =>
          val orderItem = OrderItem(product, quantity)
          val input     = result.Ok(orderItem).withErr[String]
          assert(
            result.andThen((oi: OrderItem) => result.Ok(Product(oi.product)).withErr[String])(input)
          )(
            equalTo(
              result.Ok(Product(product)).withErr[String]
            )
          )
        }
      },
      test("Given an Err value, then it should return the original error") {
        val result: result[String, Unit] = result.Err("Whamo!")
        assert(result.andThen((_: Unit) => result.Ok(42))(result))(
          equalTo(result.Err("Whamo!").withOk[Int])
        )
      }
    ),
    suite("Calling mapError as a method")(
      test("Given an Ok value, then it should return that success value") {
        val original: result[String, Int] = result.Ok(42)
        assert(original.mapError(e => "[" + e.toUpperCase() + "]"))(
          equalTo(result.Ok(42))
        )
      },
      test("Given an Err value, then it should return a mapped value") {
        val original: result[String, Int] = result.Err("Boom!!!")
        assert(original.mapError(e => "[" + e.toUpperCase() + "]"))(
          equalTo(result.Err("[BOOM!!!]"))
        )
      }
    ),
    suite("Calling mapError as a function")(
      test("Given an Ok value, then it should return that success value") {
        val original: result[String, Int] = result.Ok(42)
        assert(
          result.mapError((e: String) => "[" + e.toUpperCase() + "]")(original)
        )(
          equalTo(result.Ok(42))
        )
      },
      test("Given an Err value, then it should return a mapped value") {
        val original: result[String, Int] = result.Err("Boom!!!")
        assert(original.mapError(e => "[" + e.toUpperCase() + "]"))(
          equalTo(result.Err("[BOOM!!!]"))
        )
      }
    )
  )

  case class OrderItem(product: String, quantity: Int)
  case class Product(value: String) extends AnyVal
}
