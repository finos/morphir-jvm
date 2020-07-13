package org.morphir.sdk

import zio.duration._
import zio.test._
import zio.test.Assertion._
import zio.test.TestAspect.timeout

object ListSpec extends DefaultRunnableSpec {
  def spec = suite("ListSpec")(
    suite("list.all spec")(
      test(
        "all - should return true if all members meet the predicate condition"
      ) {
        def isEven(n: Int) = int.modBy(2)(n) == 0
        assert(list.all(isEven)(list(2, 4)))(isTrue)
      },
      test(
        "all - should return false if any member DOES NOT meet the predicate condition"
      ) {
        def isEven(n: Int) = int.modBy(2)(n) == 0
        assert(list.all(isEven)(list(2, 3)))(isFalse)
      },
      test(
        "all - should return true if the list is empty"
      ) {
        def isEven(n: Int) = int.modBy(2)(n) == 0
        assert(list.all(isEven)(list.empty))(isTrue)
      }
    ),
    suite("list.any spec")(
      test(
        "any - should return true if any members meet the predicate condition"
      ) {
        def isEven(n: Int) = int.modBy(2)(n) == 0
        assert(list.any(isEven)(list(2, 3)))(isTrue)
      },
      test(
        "any - should return false if none of the members meet the predicate condition"
      ) {
        def isEven(n: Int) = int.modBy(2)(n) == 0
        assert(list.any(isEven)(list(1, 3)))(isFalse)
      },
      test(
        "any - should return false if the list is empty"
      ) {
        def isEven(n: Int) = int.modBy(2)(n) == 0
        assert(list.any(isEven)(list.empty))(isFalse)
      }
    ),
    suite("list.append spec")(
      test("append should combine 2 lists") {
        assert(list.append(list(1, 1, 2))(list(3, 5, 8)))(
          equalTo(list(1, 1, 2, 3, 5, 8))
        )
      }
    ),
    suite("list.concat spec")(
      test("concat - should concatenate many lists") {
        assert(list.concat(list(list(1, 2), list(3), list(4, 5))))(
          equalTo(list(1, 2, 3, 4, 5))
        )
      }
    ),
    suite("list.concatMap spec")(
      test("concatMap - should map and flatten a list") {
        def doubleIt(n: Int) = list(n * 2)
        val xs               = list(1, 2, 3, 4, 5)
        assert(list.concatMap(doubleIt)(xs))(
          equalTo(list.concat(list.map(doubleIt)(xs)))
        )
      }
    ),
    suite("list.intersperse specs")(
      test("intersperse - should place the value between all elements") {
        assert(list.intersperse("on")(list("turtles", "turtles", "turtles")))(
          equalTo(list("turtles", "on", "turtles", "on", "turtles"))
        )
      },
      test("intersperse - should place the value between all elements(2)") {
        assert(list.intersperse(",")(list("A", "B", "C", "D")))(
          equalTo(list("A", ",", "B", ",", "C", ",", "D"))
        )
      }
    ),
    suite("list.filter spec")(
      test("filter should remove items that don't satisfy the given predicate") {
        val sut            = list(1, 2, 3, 4, 5, 6)
        def isEven(n: Int) = n % 2 == 0
        assert(list.filter(isEven)(sut))(equalTo(list(2, 4, 6)))
      }
    ),
    suite("list.filterMap spec")(
      test("filterMap should filter out non-ints") {
        val sut = list("3", "hi", "12", "4th", "May")
        assert(list.filterMap(string.toInt)(sut))(equalTo(list(3, 12)))
      }
    ) @@ timeout(10.seconds),
    suite("list.foldl spec")(
      test("foldl should reduce a list from the left") {
        assert(list.foldl(list.cons[Int])(list.empty[Int])(list(1, 2, 3)))(
          equalTo(list(3, 2, 1))
        )
      }
    ),
    suite("list.foldr spec")(
      test("foldr should reduce a list from the right") {
        assert(list.foldr(list.cons[Int])(list.empty[Int])(list(1, 2, 3)))(
          equalTo(list(1, 2, 3))
        )
      }
    ),
    suite("list.map2 spec")(
      test("Given lists of the same length") {
        val xs = list(1, 2, 3, 4, 5)
        val ys = list('A', 'B', 'C', 'D', 'E')
        assert(list.map2((x: Int) => (y: Char) => (x, y))(xs)(ys))(
          equalTo(list(1 -> 'A', 2 -> 'B', 3 -> 'C', 4 -> 'D', 5 -> 'E'))
        )
      },
      test(
        "Given lists where the first list is shorter than second, it should not fail"
      ) {
        val xs = list("alice", "bob", "chuck")
        val ys = list(2, 5, 7, 8)
        assert(list.map2((x: String) => (y: Int) => (x, y))(xs)(ys))(
          equalTo(list(("alice", 2), ("bob", 5), ("chuck", 7)))
        )
      },
      test(
        "Given lists where the first list is longer than second, it should not fail"
      ) {
        val xs = list("alice", "bob", "chuck", "debbie")
        val ys = list(2, 5, 7)
        assert(list.map2((x: String) => (y: Int) => (x, y))(xs)(ys))(
          equalTo(list(("alice", 2), ("bob", 5), ("chuck", 7)))
        )
      }
    ),
    suite("list.map3 specs")(
      test("Given lists of the same length") {
        val xs = list(1, 2, 3, 4, 5)
        val ys = list('A', 'B', 'C', 'D', 'E')
        val zs = list("V", "W", "X", "Y", "Z")
        assert(
          list
            .map3((x: Int) => (y: Char) => (z: String) => s"$x$y$z")(xs)(ys)(zs)
        )(
          equalTo(list("1AV", "2BW", "3CX", "4DY", "5EZ"))
        )
      },
      test(
        "Given lists where the first list is shorter than the rest, it should not fail"
      ) {
        val xs = list("alice", "bob", "chuck")
        val ys = list(2, 5, 7, 8)
        val zs = list('F', 'M', 'M', 'F', 'F')
        assert(
          list.map3((x: String) => (y: Int) => (z: scala.Char) => (x, y, z))(
            xs
          )(ys)(zs)
        )(
          equalTo(list(("alice", 2, 'F'), ("bob", 5, 'M'), ("chuck", 7, 'M')))
        )
      },
      test(
        "Given lists where the second list is shorter than the rest, it should not fail"
      ) {
        val xs = list("alice", "bob", "chuck")
        val ys = list(2, 5)
        val zs = list('F', 'M', 'M', 'F', 'F')
        assert(
          list.map3((x: String) => (y: Int) => (z: scala.Char) => (x, y, z))(
            xs
          )(ys)(zs)
        )(
          equalTo(list(("alice", 2, 'F'), ("bob", 5, 'M')))
        )
      },
      test(
        "Given lists where the third list is shorter than the rest, it should not fail"
      ) {
        val xs = list("alice", "bob", "chuck", "debbie")
        val ys = list(2, 5, 7, 8)
        val zs = list('F', 'M', 'M')
        assert(
          list.map3((x: String) => (y: Int) => (z: scala.Char) => (x, y, z))(
            xs
          )(ys)(zs)
        )(
          equalTo(list(("alice", 2, 'F'), ("bob", 5, 'M'), ("chuck", 7, 'M')))
        )
      }
    ),
    suite("list.member spcs")(
      test(
        "member should return false when the list does not contains the item"
      ) {
        assert(list.member(9)(list(1, 2, 3, 4)))(equalTo(false))
      },
      test("member should return true when the list contains the item") {
        assert(list.member(4)(list(1, 2, 3, 4)))(equalTo(true))
      }
    ),
    suite("list.reverse specs")(
      test("reverse should reverse a list") {
        assert(list.reverse(list(1, 2, 3, 4)))(equalTo(list(4, 3, 2, 1)))
      }
    )
  )
}
