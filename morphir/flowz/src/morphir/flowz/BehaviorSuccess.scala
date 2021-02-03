package morphir.flowz

final case class BehaviorSuccess[+S, +A](state: S, result: A) { self =>
  def flatMap[State2, Value2](f: A => BehaviorSuccess[State2, Value2]): BehaviorSuccess[State2, Value2] =
    f(result)

  def map[B](f: A => B): BehaviorSuccess[S, B] =
    copy(result = f(self.result))

  def mapState[S1](f: S => S1): BehaviorSuccess[S1, A] =
    copy(state = f(self.state))

  def transform[S1, B](
    f: BehaviorSuccess[S, A] => BehaviorSuccess[S1, B]
  ): BehaviorSuccess[S1, B] =
    f(self)

  def zip[S1, B](
    that: BehaviorSuccess[S1, B]
  ): BehaviorSuccess[(S, S1), (A, B)] =
    BehaviorSuccess(state = (state, that.state), result = (result, that.result))
}

object BehaviorSuccess {

  implicit def behaviorSuccessFromPair[State, Value](pair: (State, Value)): BehaviorSuccess[State, Value] =
    BehaviorSuccess(state = pair._1, result = pair._2)

  def fromPair[State, Value](pair: (State, Value)): BehaviorSuccess[State, Value] =
    BehaviorSuccess(state = pair._1, result = pair._2)
}
