package morphir.flowz

import zio.ZIO

/**
 * A `Step` that always successfully returns the same `State` and `Value`.
 */
final case class ConstantStep[+State, +Value](
  state: State,
  value: Value,
  name: Option[String],
  description: Option[String]
) extends Step[Any, State, Any, Any, Nothing, Value] {

  protected val effect: ZIO[StepContext[Any, Any, Any], Nothing, StepOutputs[State, Value]] =
    ZIO.succeed(StepOutputs(state = state, value = value))

  def named(name: String): Step[Any, State, Any, Any, Nothing, Value] = copy(name = Option(name))

  def describe(description: String): Step[Any, State, Any, Any, Nothing, Value] =
    copy(description = Option(description))
}

object ConstantStep {
  def succeed[Value](value: => Value): ConstantStep[Unit, Value] =
    ConstantStep(state = (), value = value, name = None, description = None)

  def succeed[State, Value](state: => State, value: => Value): ConstantStep[State, Value] =
    ConstantStep(state = state, value = value, name = None, description = None)

  def withStateAs[State](state: => State): ConstantStep[State, Unit] =
    ConstantStep(state = state, value = (), name = None, description = None)

  def withValue[Value](value: Value): ConstantStep[Unit, Value] =
    ConstantStep(state = (), value = value, name = None, description = None)

  def withOutputs[A](valueAndSate: A): ConstantStep[A, A] =
    succeed(state = valueAndSate, value = valueAndSate)

}
