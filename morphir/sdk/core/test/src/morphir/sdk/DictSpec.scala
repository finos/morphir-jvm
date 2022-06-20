package morphir.sdk

import zio.test.Assertion._
import zio.test._

object DictSpec extends DefaultRunnableSpec {
  def spec = suite("ListSpec")(
    suite("Dict.empty spec")(
      test("should create an empty dict") {
        assert(Dict.empty)(equalTo(Map.empty))
      }
    ),
    suite("Dict.singleton spec")(
      test("should create a dict with one single key value pair") {
        assert(Dict.singleton(1)("A"))(equalTo(Map(1 -> "A")))
      }
    ),
    suite("Dict.insert spec")(
      test("should insert a new field into a dict") {
        assert(Dict.insert(2)("B")(Map(1 -> "A")))(equalTo(Map(1 -> "A", 2 -> "B")))
      }
    ),
    suite("Dict.update spec")(
      test("should update value of a key") {
        assert(Dict.update(1)("C")(Map(1 -> "A")))(equalTo(Map(1 -> "C")))
      }
    ),
    suite("Dict.remove spec")(
      test("should remove particular key and value") {
        assert(Dict.remove(1)(Map(1 -> "A", 2 -> "B")))(equalTo(Map(2 -> "B")))
      }
    ),
    suite("Dict.isEmpty spec")(
      test("should return true if a dict is empty") {
        assert(Dict.isEmpty(Map.empty))(isTrue)
      },
      test("should return false if a dict is not empty") {
        assert(Dict.isEmpty(Map(1 -> "A", 2 -> "B")))(isFalse)
      }
    ),
    suite("Dict.member spec")(
      test("should contain given key") {
        assert(Dict.member(1)(Map(1 -> "A")))(isTrue)
      }
    ),
    suite("Dict.get spec")(
      test("should return value of dict given the key") {
        assert(Dict.get(1)(Map(1 -> "A")))(equalTo(Maybe.Just("A")))
      }
    ),
    suite("Dict.size spec")(
      test("should return the size of dict") {
        assert(Dict.size(Map(1 -> "A", 2 -> "B", 3 -> "C")))(equalTo(3))
      }
    ),
    suite("Dict.keys spec")(
      test("should return a list of keys in dict") {
        assert(Dict.keys(Map(1 -> "A", 2 -> "B", 3 -> "C")))(equalTo(List(1, 2, 3)))
      }
    ),
    suite("Dict.values spec")(
      test("should return a list of values in dict") {
        assert(Dict.values(Map(1 -> "A", 2 -> "B", 3 -> "C")))(equalTo(List("A", "B", "C")))
      }
    ),
    suite("Dict.toList spec")(
      test("should convert a dict to a list") {
        assert(Dict.toList(Map(1 -> "A", 2 -> "B", 3 -> "C")))(equalTo(List((1 -> "A"), (2 -> "B"), (3 -> "C"))))
      }
    ),
    suite("Dict.fromList spec")(
      test("should convert a list to a dict") {
        assert(Dict.fromList(List((1 -> "A"), (2 -> "B"), (3 -> "C"))))(equalTo(Map(1 -> "A", 2 -> "B", 3 -> "C")))
      }
    ),
    suite("Dict.union spec")(
      test("union of two dicts") {
        assert(Dict.union(Map(1 -> "A", 2 -> "B", 3 -> "C"))(Map(4 -> "D", 5 -> "E", 6 -> "F")))(
          equalTo(Map(1 -> "A", 2 -> "B", 3 -> "C", 4 -> "D", 5 -> "E", 6 -> "F"))
        )
      }
    ),
    suite("Dict.intersect spec")(
      test("should return common field in two dicts") {
        assert(Dict.intersect(Map(1 -> "A", 2 -> "B", 3 -> "C", 4 -> "D"))(Map(4 -> "D", 1 -> "A")))(
          equalTo(Map(4 -> "D", 1 -> "A"))
        )
      }
    ),
    suite("Dict.diff spec")(
      test("should return diff in two dicts") {
        assert(Dict.diff(Map(1 -> "A", 2 -> "B", 3 -> "C", 4 -> "D"))(Map(4 -> "D", 1 -> "A")))(
          equalTo(Map(2 -> "B", 3 -> "C"))
        )
      }
    )
  )
}
