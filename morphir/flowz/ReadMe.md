# Flowz

The Flowz module in morphir, provides a library for functionally composing small units of executable content.

At its core a `Flow` allows you to model a potentially stateful operation in a pure functional way.

In many ways a `Flow` can be looked at as a function with the following signature:

```scala
type Flow[StateIn, StateOut, Environment, Parameters, Error, Value] =
  (Environment, StateIn, Parameters) => Either[Error, (StateOut, Value)]
```

> NOTE: While strictly speaking, this is not actually how a Flow is defined, it is, however, an appropriate
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

def flow(environment: Environment, state:InputState, params:Parameters) : Either[Error, (OutputState,)]
                                                          )
```

In Elm:
```elm
flow: environment -> inputState -> parameters -> Result error (outputState, value)
```

## Inputs

The inputs of a flow form its context `FlowContext`:
- `Environment` - The environment of the flow
  - This is where services/`ZIO` modules that your `Flow` uses can be stored
- `StateIn` - The input state of the flow
  - This is an input channel which is meant to receive common data shared across flows.
- `Params` - The parameters of the flow
  - This is an input channel which is meant to receive the parameters for the flow.
    

## Outputs

The outputs of a flow are as follows:

- `Error` - The error channel of the flow (see below for further details)
- `StateOut` - The output state of the flow
- `Value` - The primary value produced by the flow

In many ways, you can think of the outputs of a flow as being a flow's output **channels**.

These channels form a flow's outbound interface to the world.

### Success Channels

If a `Flow` succeeds, it will produce data on its success channels.

Currently, upon succeeding, a flow will return an output state and a value.

> NOTE: If you have no output state, you can always return `Unit`.

### Error Channel

A `Flow` is intended to be free of side effects, and throwing errors/exceptions is one example of a potential
side effect. With that thought in mind, flowz allows you to define the type of errors a `Flow` my potentially
produce.

## Creating a Flow

There are quite a few ways one can create a flow.

You can create a flow that always succeeds with a value.

```scala
val constFlow = Flow.succeed(42)
```

You can create a flow that always fails.

```scala
val failingFlow = Flow.fail("Wah Wah!")
/// Or failing with a Throwable, such as
val errorFlow = Flow.fail(new Exception("Things fall apart."))
```
