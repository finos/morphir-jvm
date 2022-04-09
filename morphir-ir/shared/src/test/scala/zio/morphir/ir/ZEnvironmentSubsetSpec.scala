package zio.morphir.ir

import zio.morphir.testing.MorphirBaseSpec
import zio.prelude.Debug
import zio.test._

object ZEnvironmentSubsetSpec extends MorphirBaseSpec {
  def spec: ZSpec[Environment, Failure] = suite("ZEnvironmentSubset")(
    test("It should be possible to add an empty environment") {
      val env = ZEnvironmentSubset.empty
      assertTrue(env.size == 0)
    },
    test("It should be possible to place a service in the environment") {
      val env = ZEnvironmentSubset.make[Debug](TestServiceA("test"))
      assertTrue(env.size == 1)
    }
  )

  trait TestServiceA {
    def test: String
  }
  object TestServiceA {
    import Debug._
    def apply(test: String): TestServiceA = TestServiceAImpl(test)
    final case class TestServiceAImpl(test: String) extends TestServiceA

    implicit val TestServiceADebug: Debug[TestServiceA] =
      svc => Repr.VConstructor(List("zio", "morphir", "ir"), "TestServiceA", List(svc.test))
  }
}
