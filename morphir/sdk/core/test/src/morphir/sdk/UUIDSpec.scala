package morphir.sdk

import morphir.testing.MorphirBaseSpec
import zio.test._
import zio.test.Assertion._

object UUIDSpec extends MorphirBaseSpec {
  def spec = suite("UUID Tests") {
    test("Generate V3 UUID") {
      val namespace = UUID.V4.random
      val name      = "Test Name!"
      val u         = UUID.V3(namespace, name)
      println(u)
      assertTrue(true)
    }
//    test("Generate V5 UUID") {
//      val namespace = UUID.V1.next()
//      val name      = "Test Name!"
//      val u         = UUID.V5(namespace, name)
//      println(u)
//      assertTrue(true)
//    }
  }

}
