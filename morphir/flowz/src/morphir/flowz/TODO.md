## Create a DSL for Flow Creation

Create a DSL to make it easy to create an executable flow which is similar to the DSL from Jenkins pipeline

Target State:

```scala

flow {
    setup { in =>
        // Code to setup a context
    }

    // We can either create stages
    stages {
        stage("stage1") {
          step1 >>> step2
        }
    }

    // or directly create steps
    steps {
        step()
    }
}

flow (
  context { in =>
    // Code to setup a context
  },

  // We can either create stages
  stage("stage1") {
    step1 >>> step2
  }
)

flow("")
  .setup()
  .stages()
  .run()

```

## Work on Bootstrapping a flow

Consider how we can build up a flow from its services and state.

```scala
import morphir.flowz.{StageContext, StepInputs}
def context[In,Env,State,Params]
  (makeInputs: In => StepInputs[State,Params])
  (setup: StepInputs[State,Params] => StageContext[Env,State,Params])
```

