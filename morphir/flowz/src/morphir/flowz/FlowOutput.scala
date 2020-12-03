package morphir.flowz

object FlowOutput {
  val unit: FlowOutput[Unit]                               = FlowOutputs((), ())
  def apply[Output](output: => Output): FlowOutput[Output] =
    FlowOutputs.fromOutput(output)

  def fromOutput[Output](output: => Output): FlowOutput[Output] =
    FlowOutputs.fromOutput(output)
}
