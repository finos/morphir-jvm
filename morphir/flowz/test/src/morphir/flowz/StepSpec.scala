package morphir.flowz
import zio.test._
import zio.test.Assertion._
object StepSpec extends DefaultRunnableSpec {
  def spec = suite("Step Spec")(
    suite("Construction")(
      testM("It should be possible to create a step that succeeds with a value")(
        for {
          actual <- Step.succeed(42).run
        } yield assert(actual)(equalTo(42))
      ),
      testM("It should be possible to create a step that always fails with an error")(
        assertM(Step.fail(MyError("Fails!!!")).run.run)(
          fails(equalTo(MyError("Fails!!!")))
        )
      )
    )
  )

  final case class MyError(message: String) extends Exception(message)
}
