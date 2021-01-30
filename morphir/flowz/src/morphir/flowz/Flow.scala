package morphir.flowz

import morphir.flowz.Flow.FlowCase
import zio._

final case class Flow[-In, -R, +Err, +Out](
  caseValue: FlowCase[In, R, Err, Out, Flow[In, R, Err, Out]]
)

object Flow {

  def process[In, R, Err, Out](
    label: String,
    children: ZManaged[(In, R), Err, Vector[Flow[In, R, Err, Out]]]
  ): Flow[In, R, Err, Out] =
    Flow[In, R, Err, Out](ProcessCase(label, children))

  def step[SIn, SOut, In, R, Err, Out](
    label: String,
    behavior: Behavior[SIn, SOut, In, R, Err, Out]
  ): Flow[In, R, Err, Out] = Flow(StepCase(label, behavior))

  sealed abstract class FlowCase[-In, -R, +Err, +Out, +A] { self =>
    final def map[B](f: A => B): FlowCase[In, R, Err, Out, B] = self match {
      case ProcessCase(label, children) => ProcessCase(label, children.map(_.map(f)))
      case StepCase(label, behavior)    => StepCase(label, behavior)
    }
  }

  final case class ProcessCase[-In, -R, +Err, +A](
    label: String,
    children: ZManaged[(In, R), Err, Vector[A]]
  ) extends FlowCase[In, R, Err, Nothing, A]

  final case class StepCase[-SIn, +SOut, -In, -R, +Err, +Out, A](
    label: String,
    behavior: Behavior[SIn, SOut, In, R, Err, Out]
  ) extends FlowCase[In, R, Err, Out, A]
}

object example {

  val flow: Flow[Any, Any, Nothing, Nothing] = process("hello")(
    step("step1")(Behavior.succeed(10))
  )
}
