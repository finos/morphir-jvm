package org.morphir.sdk

import zio.test._
import zio.test.Assertion._

object ListSpec extends DefaultRunnableSpec {
  def spec = suite("ListSpec")(
    suite("List.map2 spec")(
      test("Given lists of the same length") {
        val xs = List(1, 2, 3, 4, 5)
        val ys = List('A', 'B', 'C', 'D', 'E')
        assert(List.map2((x: Int) => (y: Char) => (x, y))(xs)(ys))(
          equalTo(List(1 -> 'A', 2 -> 'B', 3 -> 'C', 4 -> 'D', 5 -> 'E'))
        )
      },
      test(
        "Given lists where the first list is shorter than second, it should not fail"
      ) {
        val xs = List("alice", "bob", "chuck")
        val ys = List(2, 5, 7, 8)
        assert(List.map2((x: String) => (y: Int) => (x, y))(xs)(ys))(
          equalTo(List(("alice", 2), ("bob", 5), ("chuck", 7)))
        )
      },
      test(
        "Given lists where the first list is longer than second, it should not fail"
      ) {
        val xs = List("alice", "bob", "chuck", "debbie")
        val ys = List(2, 5, 7)
        assert(List.map2((x: String) => (y: Int) => (x, y))(xs)(ys))(
          equalTo(List(("alice", 2), ("bob", 5), ("chuck", 7)))
        )
      }
    ),
    suite("List.map3 spec")(
      test("Given lists of the same length") {
        val xs = List(1, 2, 3, 4, 5)
        val ys = List('A', 'B', 'C', 'D', 'E')
        val zs = List("V", "W", "X", "Y", "Z")
        assert(
          List
            .map3((x: Int) => (y: Char) => (z: String) => s"$x$y$z")(xs)(ys)(zs)
        )(
          equalTo(List("1AV", "2BW", "3CX", "4DY", "5EZ"))
        )
      },
      test(
        "Given lists where the first list is shorter than the rest, it should not fail"
      ) {
        val xs = List("alice", "bob", "chuck")
        val ys = List(2, 5, 7, 8)
        val zs = List('F', 'M', 'M', 'F', 'F')
        assert(
          List.map3((x: String) => (y: Int) => (z: scala.Char) => (x, y, z))(
            xs
          )(ys)(zs)
        )(
          equalTo(List(("alice", 2, 'F'), ("bob", 5, 'M'), ("chuck", 7, 'M')))
        )
      },
      test(
        "Given lists where the second list is shorter than the rest, it should not fail"
      ) {
        val xs = List("alice", "bob", "chuck")
        val ys = List(2, 5)
        val zs = List('F', 'M', 'M', 'F', 'F')
        assert(
          List.map3((x: String) => (y: Int) => (z: scala.Char) => (x, y, z))(
            xs
          )(ys)(zs)
        )(
          equalTo(List(("alice", 2, 'F'), ("bob", 5, 'M')))
        )
      },
      test(
        "Given lists where the third list is shorter than the rest, it should not fail"
      ) {
        val xs = List("alice", "bob", "chuck", "debbie")
        val ys = List(2, 5, 7, 8)
        val zs = List('F', 'M', 'M')
        assert(
          List.map3((x: String) => (y: Int) => (z: scala.Char) => (x, y, z))(
            xs
          )(ys)(zs)
        )(
          equalTo(List(("alice", 2, 'F'), ("bob", 5, 'M'), ("chuck", 7, 'M')))
        )
      }
    )
  )
}
