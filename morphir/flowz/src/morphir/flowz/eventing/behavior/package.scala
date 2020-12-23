package morphir.flowz.eventing

package object behavior {
  type AggregateBehavior[-Event, State] = ZAggregateBehavior[State, Event, State]
  val AggregateBehavior: ZAggregateBehavior.type = ZAggregateBehavior
}
