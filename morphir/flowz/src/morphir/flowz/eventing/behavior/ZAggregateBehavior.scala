package morphir.flowz.eventing.behavior

import zio._

final case class ZAggregateBehavior[-StateIn, -Event, +StateOut](
  initialState: StateOut,
  update: (StateIn, Event) => Task[StateOut]
)
object ZAggregateBehavior {
  def from[S](initialState: => S): PartialBehaviorBuilder[S] = PartialBehaviorBuilder(initialState)

  final case class PartialBehaviorBuilder[S](initialState: S) {
    def update[Event](updateFunc: (S, Event) => Task[S]): AggregateBehavior[Event, S] =
      AggregateBehavior(initialState = initialState, update = updateFunc)
  }
}
