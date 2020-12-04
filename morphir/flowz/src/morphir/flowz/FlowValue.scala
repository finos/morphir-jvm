package morphir.flowz

object FlowValue {
  val unit: FlowValue[Unit]                               = OutputChannels((), ())
  def apply[Output](output: => Output): FlowValue[Output] =
    OutputChannels.fromValue(output)

  def fromValue[Output](output: => Output): FlowValue[Output] =
    OutputChannels.fromValue(output)
}
