package morphir.flowz

final case class BehaviorResult[+State, +Value](state: State, value: Value)

object BehaviorResult {
  implicit def behaviorResultFromPair[State, Value](pair: (State, Value)): BehaviorResult[State, Value] =
    BehaviorResult(state = pair._1, value = pair._2)

  def fromPair[State, Value](pair: (State, Value)): BehaviorResult[State, Value] =
    BehaviorResult(state = pair._1, value = pair._2)
}
