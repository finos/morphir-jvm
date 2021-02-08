package morphir.flowz

import zio._

final case class Flow[-InitialState, +StateOut, -InputMsg, -Env, +Err, +Result](
  caseValue: Flow.FlowCase[
    InitialState,
    StateOut,
    InputMsg,
    Env,
    Err,
    Result,
    Flow[InitialState, StateOut, InputMsg, Env, Err, Result]
  ]
) { self =>
  import morphir.flowz.Flow._

  /**
   * Annotates each step in this flow with the specified property.
   */
  final def annotate[V](key: Property[V], value: V): Flow[InitialState, StateOut, InputMsg, Env, Err, Result] =
    transform[InitialState, StateOut, InputMsg, Env, Err, Result] {
      case p @ ProcessCase(_, _) => p // We only add annotations at the step level
      case StepCase(label, behavior, annotations) =>
        Flow.StepCase(label, behavior, annotations.annotate(key, value))
    }

  final def annotated: Flow[InitialState, StateOut, InputMsg, Env with Properties, Annotated[Err], Annotated[Result]] =
    transform[InitialState, StateOut, InputMsg, Env with Properties, Annotated[Err], Annotated[Result]] {
      case ProcessCase(label, children) => Flow.ProcessCase(label, children.mapError((_, PropertyMap.empty)))
      case StepCase(label, behavior, annotations) =>
        Flow.StepCase(label, behavior.withAnnotation, annotations)
    }

  /**
   * Run is an alternative to execute that provides information around individual Step success.
   */
  def exec[State](
    initialState: State,
    message: InputMsg
  )(implicit
    ev1: State <:< InitialState,
    ev2: StateOut <:< InitialState
  ): ZManaged[Env, Err, StepSuccess[List[State], List[Result]]] = ???
  //ZManaged.accessManaged[(Any, Any, Any)] { env => }

  def execute(
    initialState: InitialState,
    message: InputMsg
  ): ZManaged[Env, Nothing, Flow[Any, StateOut, Any, Any, Err, Result]] =
    ???

  def execute: ZManaged[(InitialState, InputMsg, Env), Nothing, Flow[Any, StateOut, Any, Any, Err, Result]] =
    ZManaged.accessManaged[(InitialState, InputMsg, Env)] { case (initialState, message, env) =>
      execute(initialState, message).provide(env)
    }

  /**
   * Transforms the flow one layer at a time.
   */
  final def transform[SIn1, SOut1, Msg1, Env1, Err1, Result1](
    f: FlowCase[
      InitialState,
      StateOut,
      InputMsg,
      Env,
      Err,
      Result,
      Flow[SIn1, SOut1, Msg1, Env1, Err1, Result1]
    ] => FlowCase[
      SIn1,
      SOut1,
      Msg1,
      Env1,
      Err1,
      Result1,
      Flow[SIn1, SOut1, Msg1, Env1, Err1, Result1]
    ]
  ): Flow[SIn1, SOut1, Msg1, Env1, Err1, Result1] = caseValue match {
    case ProcessCase(label, children) => Flow(f(ProcessCase(label, children.map(_.map(_.transform(f))))))
    case s @ StepCase(_, _, _)        => Flow(f(s))
  }

  /**
   * Updates a service in the environment of this effect.
   */
  final def updateService[M] =
    new Flow.UpdateService[InitialState, StateOut, InputMsg, Env, Err, Result, M](self)
}

object Flow {

  def process[SIn, SOut, In, R, Err, Out](
    label: String,
    children: ZManaged[(SIn, In, R), Err, Vector[Flow[SIn, SOut, In, R, Err, Out]]]
  ): Flow[SIn, SOut, In, R, Err, Out] =
    Flow(ProcessCase(label, children))

  def step[SIn, SOut, In, R, Err, Out](
    label: String,
    behavior: Step[SIn, SOut, In, R, Err, Out],
    annotations: PropertyMap
  ): Flow[SIn, SOut, In, R, Err, Out] = Flow(StepCase(label, behavior, annotations))

  sealed abstract class FlowCase[-SIn, +SOut, -In, -R, +Err, +Out, +Self] { self =>
    final def map[Self1](f: Self => Self1): FlowCase[SIn, SOut, In, R, Err, Out, Self1] = self match {
      case ProcessCase(label, children)           => ProcessCase(label, children.map(_.map(f)))
      case StepCase(label, behavior, annotations) => StepCase(label, behavior, annotations)
    }
  }

  final case class ProcessCase[-SIn, -InputMsg, -R, +Err, +Self](
    label: String,
    children: ZManaged[(SIn, InputMsg, R), Err, Vector[Self]]
  ) extends FlowCase[SIn, Nothing, InputMsg, R, Err, Nothing, Self]

  final case class StepCase[-SIn, +SOut, -InputMsg, -R, +Err, +Out](
    label: String,
    behavior: Step[SIn, SOut, InputMsg, R, Err, Out], // ~ ZIO[(SIn, InputMsg, Env), E, StepSuccess[SOut, A]]
    annotations: PropertyMap
  ) extends FlowCase[SIn, SOut, InputMsg, R, Err, Out, Nothing]

  final case class UpdateService[-SIn, +SOut, -InputMsg, -Env, +Err, +Result, M](
    private val self: Flow[SIn, SOut, InputMsg, Env, Err, Result]
  ) extends AnyVal {
    def apply[Env1 <: Env with Has[M]](
      f: M => M
    )(implicit ev: Has.IsHas[Env1], tag: Tag[M]): Flow[SIn, SOut, InputMsg, Env1, Err, Result] = ???
  }

  //ZIO[(SIn, InputMsg, Env), E, StepSuccess[SOut, A]]
}

object example {

  object behaviors {
    val behavior1 = Step.unit
    val stateful  = Step.get[List[String]]
  }

  val flow = process("init")(
    process("load data")(
      process("inner")(
        step("Get accounts")(behaviors.behavior1),
        step("Get trade file")(Step.unit)
      )
    )
  )
}
