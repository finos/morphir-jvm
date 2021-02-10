package morphir.flowz

import morphir.flowz.instrumentation.InstrumentationLogging
import zio.test._
import zio.test.Assertion._

object StepSpec extends DefaultRunnableSpec {
  def spec = suite("Step Spec")(
    suite("When constructing a Step")(
      testM("It should be possible to construct a Step that always succeeds with a given value.")(
        for {
          result <- step("step-succeed")(Step.succeed(42)).runResult
        } yield assert(result)(equalTo(42))
      ),
      testM(
        "It should be possible to construct a Step that always succeeds with a given value and honors the passed in state."
      )(
        for {
          result <- step("step-succeed")(Step.succeed(42)).run(21, ())
        } yield assert(result)(equalTo(StepSuccess(21, 42)))
      ),
      testM("It should be possible to construct a Step that always fails with a given value")(
        for {
          result <- step("step-fail")(Step.fail("Oops!")).run.run
        } yield assert(result)(fails(equalTo("Oops!")))
      ),
      testM("It should be possible to construct a Step that modifies its output given an initial state")(
        for {
          result <-
            step("step-modify")(Step.modify { text: String => s"$text:${text.size}" -> text.size }).run("Hello", ())
        } yield assert(result)(equalTo(StepSuccess("Hello:5", 5)))
      ),
      testM("It should be possible to construct a Step from a simple update function")(
        for {
          result <-
            step("step-update")(
              Step
                .update[List[String], List[String], String, String] { case (initialState: List[String], msg: String) =>
                  (msg :: initialState, msg.reverse)
                }
            )
              .run(List("John", "Joe"), "Jack")
        } yield assert(result)(equalTo(StepSuccess(state = List("Jack", "John", "Joe"), result = "kcaJ")))
      ),
      testM("It should be possible to construct a behavior that gets the initial state unchanged.")(
        for {
          result <- step("step-get")(Step.get[Set[Int]]).run(Set(1, 2, 3, 4), Set(5, 6, 7, 8))
        } yield assert(result)(equalTo(StepSuccess(Set(1, 2, 3, 4), Set(1, 2, 3, 4))))
      ),
      testM("It should be possible to construct a behavior that sets the state to a value.")(
        checkM(Gen.alphaNumericString, Gen.alphaNumericString) { (input, s1) =>
          for {
            result <- step("step-set")(Step.set(input)).run(s1, "Something")
          } yield assert(result)(equalTo(StepSuccess(state = input, result = ())))
        }
      )
    ),
    suite("Operations")(
      testM("It should be possible to return a different constant value using as")(
        for {
          result <- step("step-as")(Step.unit.as("Foo")).run("S1", ())
        } yield assert(result)(equalTo(StepSuccess("S1", "Foo")))
      )
    ),
    suite("Combining")(
      testM("It should be possible to sequence flows using flatMap") {
        val behavior = step("step-flatMap")(
          Step.succeed("true").flatMap(value => Step.succeed(s"The answer is: $value"))
        ).run(21, 21)
        assertM(behavior)(equalTo(StepSuccess(21, "The answer is: true")))
      }
    )
  ).provideCustomLayer(StepUidGenerator.live ++ InstrumentationLogging.ignore)
}
