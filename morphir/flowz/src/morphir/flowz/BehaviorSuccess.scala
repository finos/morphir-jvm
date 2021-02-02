package morphir.flowz

final case class BehaviorSuccess[+State, +Value](state: State, result: Value) { self =>
  def flatMap[State2, Value2](f: Value => BehaviorSuccess[State2, Value2]): BehaviorSuccess[State2, Value2] =
    f(result)

  def map[Value2](f: Value => Value2): BehaviorSuccess[State, Value2] =
    copy(result = f(self.result))

  def mapState[State2](f: State => State2): BehaviorSuccess[State2, Value] =
    copy(state = f(self.state))

  def transform[State2, Value2](
    f: BehaviorSuccess[State, Value] => BehaviorSuccess[State2, Value2]
  ): BehaviorSuccess[State2, Value2] =
    f(self)
}

object BehaviorSuccess {

  implicit def behaviorSuccessFromPair[State, Value](pair: (State, Value)): BehaviorSuccess[State, Value] =
    BehaviorSuccess(state = pair._1, result = pair._2)

  def fromPair[State, Value](pair: (State, Value)): BehaviorSuccess[State, Value] =
    BehaviorSuccess(state = pair._1, result = pair._2)
}
