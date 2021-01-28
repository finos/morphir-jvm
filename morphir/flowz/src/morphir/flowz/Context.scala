package morphir.flowz

sealed trait Context {
  def eventBus: EventBus
}

object Context {
  final case class FlowStartupContext(eventBus: EventBus)                            extends Context
  final case class StepInputContext[+Params](parameters: Params, eventBus: EventBus) extends Context
  final case class StepOutputContext(eventBus: EventBus)                             extends Context
}

sealed trait EventBus
