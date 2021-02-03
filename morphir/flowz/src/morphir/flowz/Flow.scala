package morphir.flowz

import morphir.flowz.Flow.FlowCase
import zio._

final case class Flow[-StateIn, +StateOut, -Msg, -Env, +Err, +Result](
  caseValue: FlowCase[StateIn, StateOut, Msg, Env, Err, Result, Flow[StateIn, StateOut, Msg, Env, Err, Result]]
) {
  def run(initialState: StateIn, message: Msg): ZIO[Env, Err, FlowSuccess[StateOut, Result]] = ???
}

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

  object behaviors {
    val behavior1 = Behavior.unit
    val stateful  = Behavior.get[List[String]]
  }

  val flow = process("init")(
    process("load data")(
      process("inner")(
        step("Get accounts")(behaviors.behavior1),
        step("Get trade file")(Behavior.unit)
      )
    )
  )
}
