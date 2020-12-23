# Flowz

The Flowz module in morphir, provides a library for functionally composing small units of executable content.

At its core flowz is about composing individual `Step`s into a workflow. A `Step` allows you to model a potentially stateful operation in a pure functional way.

In many ways a `Step` can be looked at as a function with the following signature:

```scala
type Step[StateIn, StateOut, Environment, Parameters, Error, Value] =
  (Environment, StateIn, Parameters) => Either[Error, (StateOut, Value)]
```

> NOTE: While strictly speaking, this is not actually how a Step is defined, it is, however, an appropriate
> mental model to keep in mind. 
> Another way one could potentially imagine a flow is like the following:

In Scala:
```scala
// Define inputs
type Environment
type InputState
type Parameters

type OutputState
type Value

def step(environment: Environment, state:InputState, params:Parameters) : Either[Error, (OutputState,Value)])
```

In Elm:
```elm
step: environment -> inputState -> parameters -> Result error (outputState, value)
```

## Inputs

The inputs of a Step form its context `StepContext`:
- `Environment` - The environment of the step
  - This is where services/`ZIO` modules that your `Step` uses can be stored
- `StateIn` - The input state of the step
  - This is an input channel which is meant to receive common data shared across steps.
- `Params` - The parameters of the step
  - This is an input channel which is meant to receive the parameters for the step.
    

## Outputs

The outputs of a step are as follows:

- `Error` - The error channel of the step (see below for further details)
- `StateOut` - The output state of the step
- `Value` - The primary value produced by the step

In many ways, you can think of the outputs of a step as being a flow's output **channels**.

These channels form a step's outbound interface to the world.

### Success Channels

If a `Step` succeeds, it will produce data on its success channels.

Currently, upon succeeding, a step will return an output state and a value.

> NOTE: If you have no output state, you can always return `Unit`.

### Error Channel

A `Step` is intended to be free of side effects, and throwing errors/exceptions is one example of a potential
side effect. With that thought in mind, flowz allows you to define the type of errors a `Step` my potentially
produce.

## Creating a Flow

There are quite a few ways one can create a flow.

### You can create a flow that always succeeds with a value.

```scala
val constFlow = Step.succeed(42)
```

### You can create a flow that always fails.

```scala
val failingFlow = Step.fail("Wah Wah!")
/// Or failing with a Throwable, such as
val errorFlow = Step.fail(new Exception("Things fall apart."))
```

### You can create a flow that combines one or more steps together.

```scala
// Let's start with a step that gets the optional target
val getTarget = Step.fromFunction { args: List[String] => args.headOption.map(Target) }

// Next let's construct a step that expects an optional target and prints a greeting to that target or the world
// if no target is specified
val greeterStep = Step.fromEffect { greeting: Option[Target] =>
  console.putStrLn(s"Hello, ${greeting getOrElse "world"}")
}

val helloFlow = getTarget >>> greeterStep
```

## Running a Flow

You can run a flow using it's run method which returns a `ZIO` effect. You can execute that effect like
any other `ZIO` effect:

```scala
import morphir.flowz.api._
import zio._

object HelloWorld extends App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    val helloStep = Step.fromEffect { greeting: Option[String] =>
      console.putStrLn(s"Hello, ${greeting.getOrElse("world")}")
    }

    helloStep.run(args.headOption).exitCode

  }
}
```