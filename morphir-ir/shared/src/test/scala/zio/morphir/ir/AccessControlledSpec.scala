package zio.morphir.ir
import zio.test.*
import zio.test.Assertion.*

object AccessControlledSpec extends DefaultRunnableSpec {
  def spec = suite("AccessControlled")(
    test("Supports for comprehensions") {
      val code =
        """
              for {
                  publicInt <- AccessControlled.publicAccess(10)
                  privateStr <- AccessControlled.privateAccess("hello")
              } yield (publicInt, privateStr)
              """
      assertM(typeCheck(code))(isRight)
    },
    test("Equality") {
      assertTrue(
        AccessControlled.publicAccess(10) == AccessControlled.publicAccess(10),
        AccessControlled.publicAccess(10) != AccessControlled.privateAccess(10
          ),
        AccessControlled.publicAccess(10).value == AccessControlled.privateAccess(10).value,
        AccessControlled.publicAccess(10).access != AccessControlled.privateAccess(10).access
      )
    }
  )
}
