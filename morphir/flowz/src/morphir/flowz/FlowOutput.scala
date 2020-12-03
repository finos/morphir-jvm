package morphir.flowz

object FlowOutput {
  val unit: FlowOutput[Unit]                               = FlowSuccess((), ())
  def apply[Output](output: => Output): FlowOutput[Output] =
    FlowSuccess.fromOutput(output)

  def fromOutput[Output](output: => Output): FlowOutput[Output] =
    FlowSuccess.fromOutput(output)
}
