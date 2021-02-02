package morphir.flowz

final case class BehaviorResult[+State, +Value](state: State, result: Value) { self =>
  def flatMap[State2, Value2](f: Value => BehaviorResult[State2, Value2]): BehaviorResult[State2, Value2] =
    f(result)

  def map[Value2](f: Value => Value2): BehaviorResult[State, Value2] =
    copy(result = f(self.result))

  def mapState[State2](f: State => State2): BehaviorResult[State2, Value] =
    copy(state = f(self.state))

  def transform[State2, Value2](
    f: BehaviorResult[State, Value] => BehaviorResult[State2, Value2]
  ): BehaviorResult[State2, Value2] =
    f(self)
}

object BehaviorResult {
  implicit def behaviorResultFromPair[State, Value](pair: (State, Value)): BehaviorResult[State, Value] =
    BehaviorResult(state = pair._1, result = pair._2)

  def fromPair[State, Value](pair: (State, Value)): BehaviorResult[State, Value] =
    BehaviorResult(state = pair._1, result = pair._2)
}
