package morphir.flowz.experimental

final case class ExecutedFlow[+E](caseValue: ExecutedFlow.FlowCase[E, ExecutedFlow[E]]) { self =>
  import morphir.flowz.experimental.ExecutedFlow._

  /**
   * Folds over all nodes to produce a final result.
   */
  def fold[Z](f: FlowCase[E, Z] => Z): Z =
    caseValue match {
      case ProcessCase(label, specs) => f(ProcessCase(label, specs.map((_.fold(f)))))
      case s @ StepCase(_)           => f(s)
    }

  /**
   * Computes the size of the flow, i.e. the number of steps in the flow.
   */
  def size: Int =
    fold[Int] {
      case ProcessCase(_, counts) => counts.sum
      case StepCase(_)            => 1
    }

}

object ExecutedFlow {
  sealed trait FlowCase[+E, +Self] { self =>
    def map[Self2](f: Self => Self2): FlowCase[E, Self2] =
      self match {
        case ProcessCase(label, specs) => ProcessCase(label, specs.map(f))
        case StepCase(label)           => StepCase(label)
      }
  }

  final case class ProcessCase[+Self](label: String, specs: Vector[Self]) extends FlowCase[Nothing, Self]
  final case class StepCase[+E](label: String)                            extends FlowCase[E, Nothing]

  def process[E](label: String, specs: Vector[ExecutedFlow[E]]): ExecutedFlow[E] =
    ExecutedFlow(ProcessCase(label, specs))

}
