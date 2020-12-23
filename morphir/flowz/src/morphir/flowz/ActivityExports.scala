package morphir.flowz

import zio.ZIO

trait ActivityExports { stepExports: StepExports =>

  /** Defines a step that does not rely on state. */
  def act[Params, Out](f: Params => Out): Activity[Any, Params, Throwable, Out] = Activity.act(f)

  /** Defines a step that does not rely on state. */
  def act[Params, Out](name: String)(f: Params => Out): Activity[Any, Params, Throwable, Out] =
    Activity.act(name)(f)

  /** Defines a step that does not rely on state. */
  def act[Params, Out](name: String, description: String)(f: Params => Out): Activity[Any, Params, Throwable, Out] =
    Activity.act(name = name, description = description)(f)

  /** Defines a step that performs some effect which does not rely on state. */
  def activity[Env, Params, Err, Value](func: Params => ZIO[Env, Err, Value]): Activity[Env, Params, Err, Value] =
    Activity.activity(func)

  /** Defines a step that performs some effect which does not rely on state. */
  def activity[Env, Params, Err, Value](name: String)(
    func: Params => ZIO[Env, Err, Value]
  ): Activity[Env, Params, Err, Value] = Activity.activity(name)(func)

  /** Defines a step that performs some effect which does not rely on state. */
  def activity[Env, Params, Err, Value](name: String, description: String)(
    func: Params => ZIO[Env, Err, Value]
  ): Activity[Env, Params, Err, Value] = Activity.activity(name = name, description = description)(func)

  object Activity {
    def apply[Env, Params, Err, Value](f: Params => ZIO[Env, Err, Value]): Activity[Env, Params, Err, Value] =
      activity(f)

    /** Defines a step that does not rely on state. */
    def act[Params, Out](f: Params => Out): Activity[Any, Params, Throwable, Out] =
      Step.fromFunction(f)

    /** Defines a step that does not rely on state. */
    def act[Params, Out](name: String)(f: Params => Out): Activity[Any, Params, Throwable, Out] =
      Step.fromFunction(f).named(name)

    /** Defines a step that does not rely on state. */
    def act[Params, Out](name: String, description: String)(f: Params => Out): Activity[Any, Params, Throwable, Out] =
      Step.fromFunction(f).copy(name = Option(name), description = Option(description))

    /** Defines a step that performs some effect which does not rely on state. */
    def activity[Env, Params, Err, Value](func: Params => ZIO[Env, Err, Value]): Activity[Env, Params, Err, Value] =
      Step(ZIO.accessM[StepContext[Env, Any, Params]] { ctx =>
        func(ctx.inputs.params).map(value => StepOutputs(state = value, value = value)).provide(ctx.environment)
      })

    /** Defines a step that performs some effect which does not rely on state. */
    def activity[Env, Params, Err, Value](name: String)(
      func: Params => ZIO[Env, Err, Value]
    ): Activity[Env, Params, Err, Value] =
      Step(
        ZIO.accessM[StepContext[Env, Any, Params]] { ctx =>
          func(ctx.inputs.params).map(value => StepOutputs(state = value, value = value)).provide(ctx.environment)
        },
        name = Option(name)
      )

    def activity[Env, Params, Err, Value](name: String, description: String)(
      func: Params => ZIO[Env, Err, Value]
    ): Activity[Env, Params, Err, Value] =
      Step(
        ZIO.accessM[StepContext[Env, Any, Params]] { ctx =>
          func(ctx.inputs.params).map(value => StepOutputs(state = value, value = value)).provide(ctx.environment)
        },
        name = Option(name),
        description = Option(description)
      )

    def describe[Env, Params, Err, Value](description: String)(
      activity: Activity[Env, Params, Err, Value]
    ): Activity[Env, Params, Err, Value] =
      Step.describe(description)(activity)

    def environment[Env]: Activity[Env, Any, Nothing, Env] = Step.environment[Env]

    def name[Env, Params, Err, Value](name: String)(
      activity: Activity[Env, Params, Err, Value]
    ): Activity[Env, Params, Err, Value] =
      Step.name(name)(activity)

    def named[Env, Params, Err, Value](name: String)(
      activity: Activity[Env, Params, Err, Value]
    ): Activity[Env, Params, Err, Value] =
      Step.name(name)(activity)

    def parameters[Params]: Activity[Any, Params, Nothing, Params] = Step.parameters[Params]

  }
}
