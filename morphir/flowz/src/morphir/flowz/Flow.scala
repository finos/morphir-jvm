package morphir.flowz

import morphir.flowz.Flow.FlowCase
import zio._

final case class Flow[-SIn, +SOut, -In, -R, +Err, +Out](
  caseValue: FlowCase[SIn, SOut, In, R, Err, Out, Flow[SIn, SOut, In, R, Err, Out]]
)

object Flow {

  def process[SIn, SOut, In, R, Err, Out](
    label: String,
    children: ZManaged[(SIn, In, R), Err, Vector[Flow[SIn, SOut, In, R, Err, Out]]]
  ): Flow[SIn, SOut, In, R, Err, Out] =
    Flow(ProcessCase(label, children))

  def step[SIn, SOut, In, R, Err, Out](
    label: String,
    behavior: Behavior[SIn, SOut, In, R, Err, Out]
  ): Flow[SIn, SOut, In, R, Err, Out] = Flow(StepCase(label, behavior))

  sealed abstract class FlowCase[-SIn, +SOut, -In, -R, +Err, +Out, +A] { self =>
    final def map[B](f: A => B): FlowCase[SIn, SOut, In, R, Err, Out, B] = self match {
      case ProcessCase(label, children) => ProcessCase(label, children.map(_.map(f)))
      case StepCase(label, behavior)    => StepCase(label, behavior)
    }
  }

  final case class ProcessCase[-SIn, -In, -R, +Err, +A](
    label: String,
    children: ZManaged[(SIn, In, R), Err, Vector[A]]
  ) extends FlowCase[SIn, Nothing, In, R, Err, Nothing, A]

  final case class StepCase[-SIn, +SOut, -In, -R, +Err, +Out, A](
    label: String,
    behavior: Behavior[SIn, SOut, In, R, Err, Out]
  ) extends FlowCase[SIn, SOut, In, R, Err, Out, A]
}

object example {

  val flow = process("hello")(
    step("step2")(outputting(state = List("Oops"), value = 42))
  )
}
