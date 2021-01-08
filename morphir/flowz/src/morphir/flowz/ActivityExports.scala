package morphir.flowz

import zio._

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

}
