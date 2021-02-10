package morphir.flowz

final case class FlowSuccess[+S, +A](state: S, result: A) {}
object FlowSuccess {
  def fromResult[S, A](result: StepSuccess[S, A]): FlowSuccess[S, A] =
    FlowSuccess(state = result.state, result = result.result)
}
