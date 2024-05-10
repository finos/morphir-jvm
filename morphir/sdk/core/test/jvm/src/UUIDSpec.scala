import morphir.testing.MorphirBaseSpec
import zio.test.*
import zio.test.Assertion.*
import memeid4s.{ UUID => MUUID }

object UUIDSpec extends MorphirBaseSpec {
  def spec = suite("UUID Tests")(
    test("Generate UUID from valid String") {
      val uuid = UUID.V4.random
      assert(UUID.from(uuid.toString))(isRight(equalTo(uuid)))
    },
    test("Return Throwable from invalid String") {
      assert(UUID.from("0f769c4185a634208b09bb63bce12014"))(isLeft)
    },
    test("UUID from msb and lsb") {
      check(Gen.long) { (msb) =>
        assertEquals(UUID.from(msb, 0xc000000000000000L), MUUID.from(msb, 0xc000000000000000L))
      }
    },
    suite("V1 UUID")(
      test("Generate unique V1 UUIDs") {
        assert((1 to 999).map(_ => UUID.V1.next()).toSet.size)(equalTo(999))
      }
    ),
    suite("V3 UUID")(
      test("Generate same V3 UUID for same namespace and name") {
        val namespace = UUID.V4.random
        val name      = "Test Name!"
        val u1        = UUID.V3(namespace, name)
        val u2        = UUID.V3(namespace, name)
        assertEquals(u1, u2)
      },
      test("Generate unique V3 UUID for unique namespace") {
        val namespace1 = UUID.V4.random
        val namespace2 = UUID.V4.random
        val name       = "Test Name!"
        val u1         = UUID.V3(namespace1, name)
        val u2         = UUID.V3(namespace2, name)
        assertTrue(u1 != u2)
      },
      test("Generate unique V3 UUID for unique names") {
        val namespace = UUID.V4.random
        val name1     = "Test Name!"
        val name2     = "Test Name 2!"
        val u1        = UUID.V3(namespace, name1)
        val u2        = UUID.V3(namespace, name2)
        assertTrue(u1 != u2)
      }
    ),
    suite("V4 UUID")(
      test("Generate Uniquely Random V4 UUIDs") {
        assert((1 to 999).map(_ => UUID.V4.random).toSet.size)(equalTo(999))
      }
    ),
    suite("V5 UUID")(
      test("Generate same V5 UUID for same namespace and name") {
        val namespace = UUID.V4.random
        val name      = "Test Name!"
        val u1        = UUID.V5(namespace, name)
        val u2        = UUID.V5(namespace, name)
        assertEquals(u1, u2)
      },
      test("Generate unique V5 UUID for unique namespace") {
        val namespace1 = UUID.V4.random
        val namespace2 = UUID.V4.random
        val name       = "Test Name!"
        val u1         = UUID.V5(namespace1, name)
        val u2         = UUID.V5(namespace2, name)
        assertTrue(u1 != u2)
      },
      test("Generate unique V5 UUID for unique names") {
        val namespace = UUID.V4.random
        val name1     = "Test Name!"
        val name2     = "Test Name 2!"
        val u1        = UUID.V5(namespace, name1)
        val u2        = UUID.V5(namespace, name2)
        assertTrue(u1 != u2)
      }
    )
  )

}
