package morphir.flowz

final case class BehaviorResult[+State, +Value](state: State, value: Value) { self =>
  def map[Value2](f: Value => Value2): BehaviorResult[State, Value2] =
    copy(value = f(self.value))
}

object BehaviorResult {
  implicit def behaviorResultFromPair[State, Value](pair: (State, Value)): BehaviorResult[State, Value] =
    BehaviorResult(state = pair._1, value = pair._2)

  def fromPair[State, Value](pair: (State, Value)): BehaviorResult[State, Value] =
    BehaviorResult(state = pair._1, value = pair._2)
}
