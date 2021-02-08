package morphir.flowz

final case class StepSuccess[+S, +A](state: S, result: A) { self =>
  def flatMap[State2, Value2](f: A => StepSuccess[State2, Value2]): StepSuccess[State2, Value2] =
    f(result)

  def map[B](f: A => B): StepSuccess[S, B] =
    copy(result = f(self.result))

  def mapState[S1](f: S => S1): StepSuccess[S1, A] =
    copy(state = f(self.state))

  def toTuple: (S, A) = (state, result)

  def transform[S1, B](
    f: StepSuccess[S, A] => StepSuccess[S1, B]
  ): StepSuccess[S1, B] =
    f(self)

  def zip[S1, B](
    that: StepSuccess[S1, B]
  ): StepSuccess[(S, S1), (A, B)] =
    StepSuccess(state = (state, that.state), result = (result, that.result))
}

object StepSuccess {

  implicit def behaviorSuccessFromPair[State, Value](pair: (State, Value)): StepSuccess[State, Value] =
    StepSuccess(state = pair._1, result = pair._2)

  def fromPair[State, Value](pair: (State, Value)): StepSuccess[State, Value] =
    StepSuccess(state = pair._1, result = pair._2)
}
