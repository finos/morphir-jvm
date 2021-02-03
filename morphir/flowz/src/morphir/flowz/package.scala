package morphir

import zio._
import zio.prelude._

import scala.collection.immutable.SortedSet

package object flowz {
  object api extends Api

  type Annotations   = Has[Annotations.Service]
  type Annotated[+A] = (A, StepAnnotationMap)

  type Activity[-Env, -Params, +Err, +Value] = Act[Any, Value, Env, Params, Err, Value]
  type IOAct[-Params, +Err, +Value]          = Act[Any, Unit, Any, Params, Err, Value]
  type TaskAct[-Params, +Value]              = Act[Any, Unit, Any, Params, Throwable, Value]
  type UAct[-Params, +Value]                 = Act[Any, Any, Any, Params, Nothing, Value]

  object CommandLineArgs extends Subtype[List[String]]
  type CommandLineArgs = CommandLineArgs.Type

  object Variables extends Subtype[Map[String, String]]
  type Variables = Variables.Type

  type FlowHostContext[+R] = (R, CommandLineArgs, Variables)

  type UFlowHost[+HostParams] = FlowHost[Any, Nothing, HostParams]

  type ForkedStep[-StateIn, +StateOut, -Env, -Params, +Err, +Output] =
    Act[StateIn, Unit, Env, Params, Nothing, Fiber.Runtime[Err, StepOutputs[StateOut, Output]]]

  type BehaviorEffect[-SIn, +SOut, -Msg, -Env, +E, +A] = ZIO[(SIn, Msg, Env), E, BehaviorSuccess[SOut, A]]

  /**
   * Provides a description of an independent behavior which does not
   * rely on any inputs to produce its outputs.
   */
  type IndieBehavior[+S, +E, +A] = Behavior[Any, S, Any, Any, E, A]

//  def behavior[InputState, OutputState, Msg, R, Err, A](
//    f: (InputState, Msg) => ZIO[R, Err, (OutputState, A)]
//  )(implicit ev: CanFail[Err]): Behavior[InputState, OutputState, Msg, R, Err, A] =
//    Behavior[InputState, OutputState, Msg, R, Err, A](f)

//  def behavior[SIn, OutputState, Msg, R, E, A](
//    effect: ZIO[R with InputState[SIn], E, A]
//  ): Behavior[SIn, OutputState, Msg, R, Nothing, A] =
//    Behavior[SIn, OutputState, Msg, R, E, A](effect)

  def behavior[InputState, OutputState, Msg, R, A](
    f: (InputState, Msg) => URIO[R, BehaviorSuccess[OutputState, A]]
  ): Behavior[InputState, OutputState, Msg, R, Nothing, A] =
    Behavior.behaviorFromFunctionM(f)

  def outputting[OutputState, Value](state: OutputState, value: Value): IndieBehavior[OutputState, Nothing, Value] =
    Behavior.outputting(state, value)

  def process[SIn, SOut, In, R, Err, Out](label: String)(
    children: Flow[SIn, SOut, In, R, Err, Out]*
  ): Flow[SIn, SOut, In, R, Err, Out] =
    Flow.process(label, ZManaged.succeed(children.toVector))

  def step[SIn, SOut, In, R, Err, Out](label: String)(
    behavior: Behavior[SIn, SOut, In, R, Err, Out]
  ): Flow[SIn, SOut, In, R, Err, Out] = Flow.step(label, behavior, StepAnnotationMap.empty)

  /**
   * The `Annotations` trait provides access to an annotation map that tests
   * can add arbitrary annotations to. Each annotation consists of a string
   * identifier, an initial value, and a function for combining two values.
   * Annotations form monoids and you can think of `Annotations` as a more
   * structured logging service or as a super polymorphic version of the writer
   * monad effect.
   */
  object Annotations {

    trait Service extends Serializable {
      def annotate[V](key: StepAnnotation[V], value: V): UIO[Unit]
      def get[V](key: StepAnnotation[V]): UIO[V]
      def withAnnotation[R, E, A](zio: ZIO[R, E, A]): ZIO[R, Annotated[E], Annotated[A]]
      def supervisedFibers: UIO[SortedSet[Fiber.Runtime[Any, Any]]]
    }

    /**
     * Accesses an `Annotations` instance in the environment and appends the
     * specified annotation to the annotation map.
     */
    def annotate[V](key: StepAnnotation[V], value: V): URIO[Annotations, Unit] =
      ZIO.accessM(_.get.annotate(key, value))

    /**
     * Accesses an `Annotations` instance in the environment and retrieves the
     * annotation of the specified type, or its default value if there is none.
     */
    def get[V](key: StepAnnotation[V]): URIO[Annotations, V] =
      ZIO.accessM(_.get.get(key))

    /**
     * Constructs a new `Annotations` service.
     */
    val live: Layer[Nothing, Annotations] =
      ZLayer.fromEffect(FiberRef.make(StepAnnotationMap.empty).map { fiberRef =>
        new Annotations.Service {
          def annotate[V](key: StepAnnotation[V], value: V): UIO[Unit] =
            fiberRef.update(_.annotate(key, value))
          def get[V](key: StepAnnotation[V]): UIO[V] =
            fiberRef.get.map(_.get(key))
          def withAnnotation[R, E, A](zio: ZIO[R, E, A]): ZIO[R, Annotated[E], Annotated[A]] =
            fiberRef.locally(StepAnnotationMap.empty) {
              zio.foldM(e => fiberRef.get.map((e, _)).flip, a => fiberRef.get.map((a, _)))
            }
          def supervisedFibers: UIO[SortedSet[Fiber.Runtime[Any, Any]]] =
            ZIO.descriptorWith { descriptor =>
              get(StepAnnotation.fibers).flatMap {
                case Left(_) =>
                  ZIO.succeed(SortedSet.empty[Fiber.Runtime[Any, Any]]) //TODO: Possible to do succeedNow????
                case Right(refs) =>
                  ZIO
                    .foreach(refs)(_.get)
                    .map(_.foldLeft(SortedSet.empty[Fiber.Runtime[Any, Any]])(_ ++ _))
                    .map(_.filter(_.id != descriptor.id))
              }
            }
        }
      })

    /**
     * Accesses an `Annotations` instance in the environment and executes the
     * specified effect with an empty annotation map, returning the annotation
     * map along with the result of execution.
     */
    def withAnnotation[R <: Annotations, E, A](zio: ZIO[R, E, A]): ZIO[R, Annotated[E], Annotated[A]] =
      ZIO.accessM(_.get.withAnnotation(zio))

    /**
     * Returns a set of all fibers in this test.
     */
    def supervisedFibers: ZIO[Annotations, Nothing, SortedSet[Fiber.Runtime[Any, Any]]] =
      ZIO.accessM(_.get.supervisedFibers)
  }
}
