package morphir.flowz

import zio.test._
import zio.test.Assertion._
import zio.test.environment.TestSystem

object StepSpec extends DefaultRunnableSpec {
  def spec = suite("Step Spec")(
    suite("Constructing")(
      testM("It should be possible to create a flow that always succeeds with the unit value")(
        for {
          output <- Step.unit.run
        } yield assert(output)(equalTo(StepOutputs.unit))
      ),
      testM("It should be possible to create a flow that always succeeds with None")(
        for {
          output <- Step.none.run
        } yield assert(output)(equalTo(StepOutputs.none))
      ),
      testM("It should be possible to create a flow that always succeeds with a value")(
        for {
          output <- Step.succeed(42).run
        } yield assert(output)(equalTo(StepOutputs.fromValue(42)))
      ),
      testM(
        "It should be possible to create a flow that always succeeds with a value (and it should pass-thru the state)"
      )(
        for {
          output <- Step.succeed(42).run("Hello", List("--help"))
        } yield assert(output)(equalTo(StepOutputs(value = 42, state = List("--help"))))
      ),
      testM("It should be possible to create a flow that always succeeds with the given output and state")(
        for {
          actual <- Step.succeedWith(value = 42, state = "What is the answer?").run
        } yield assert(actual)(equalTo(StepOutputs(state = "What is the answer?", value = 42)))
      ),
      testM("It should be possible to create a flow that always fails with a value")(
        for {
          result <- Step.fail("NO!!!").run.run
        } yield assert(result)(fails(equalTo("NO!!!")))
      ),
      testM("It should be possible to create a flow that produces the value of executing a function")(
        checkM(Gen.int(1, 5000)) { input =>
          for {
            actual  <- Step.fromFunction { n: Int => n * 2 }.run(input)
            expected = StepOutputs.assignBoth(input * 2)
          } yield assert(actual)(equalTo(expected))
        }
      ),
      testM("It should be possible to create a flow from an Option when the value is a Some")(
        for {
          actual <- Step.fromOption(Some(Widget("sprocket"))).run.map(_.value)
        } yield assert(actual)(equalTo(Widget("sprocket")))
      ),
      testM("It should be possible to create a flow from an Option when the value is a None")(
        for {
          actual <- Step.fromOption(None).run.map(_.value).run
        } yield assert(actual)(fails(isNone))
      )
    ),
    suite("Combining")(
      testM("It should be possible to combine steps using the >>> operator.") {
        val start  = Step.parameters[List[String]]
        val next   = Step.stateful((_: Any, args: List[String]) => (args, args.headOption))
        val myStep = start >>> next
        assertM(myStep.run(List("Hello", "World")))(
          equalTo(StepOutputs(state = List("Hello", "World"), value = Option("Hello")))
        )
      },
      testM("The parameters constructor should pass through the state it is given") {
        val givenState = List("A", "B", "C")
        val givenParam = "Hello"
        val sut        = Step.parameters[String]
        assertM(sut.run(givenParam, givenState))(
          equalTo(StepOutputs(state = givenState, value = givenParam))
        )
      },
      testM("It should be possible to combine flows using zip") {
        val flowA                                                            = Step.withStateAndValue("A")
        val flowB                                                            = Step.withStateAndValue(1)
        val flow: Step[Any, (String, Int), Any, Any, Nothing, (String, Int)] = flowA zip flowB
        assertM(flow.run)(equalTo(StepOutputs(state = ("A", 1), value = ("A", 1))))
      },
      testM("It should be possible to combine flows using the zip operator <*>") {
        val flowA                                                            = Step.withStateAndValue("A")
        val flowB                                                            = Step.withStateAndValue(1)
        val flow: Step[Any, (String, Int), Any, Any, Nothing, (String, Int)] = flowA <*> flowB
        assertM(flow.run)(equalTo(StepOutputs(state = ("A", 1), value = ("A", 1))))
      },
      testM("It should be possible to sequence flows using flatMap") {
        val flow = Step.succeed("true").flatMap(value => Step.succeed(s"The answer is: $value"))
        assertM(flow.run)(equalTo(StepOutputs.fromValue("The answer is: true")))
      },
      testM("It should be possible to sequence flows using a for comprehension") {
        for {
          _ <- TestSystem.putEnv("PROFILE", "local")
          out <- (for {
                   cfg             <- Step.succeed(Map("profile.local.host" -> "127.0.0.1", "profile.default.host" -> "finos.org"))
                   selectedProfile <- Step.fromEffect(zio.system.envOrElse("PROFILE", "default"))
                   host            <- Step.succeed(cfg.getOrElse(s"profile.$selectedProfile.host", "morphir.org"))
                 } yield host).run.map(_.value)
        } yield assert(out)(equalTo("127.0.0.1"))
      }
    ),
    testM("It should be possible to rename a step without affecting its value") {
      val theStep = Step.succeed("Good Boy!")
      val named   = Step.name("Praise")(theStep)

      for {
        original <- theStep.run
        actual   <- named.run
      } yield assert(actual)(equalTo(original))
    },
    testM("It should be possible to rename a step without affecting its value") {
      val theStep = Step.succeed("Good Boy!")
      val named   = Step.name("Praise")(theStep)

      for {
        original <- theStep.run
        actual   <- named.run
      } yield assert(actual)(equalTo(original))
    }
  )

  final case class Widget(name: String)
}
