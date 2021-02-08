package morphir

import morphir.flowz.instrumentor.Instrumentor
import zio._
import zio.clock.Clock
import zio.duration.Duration
import zio.prelude._

import scala.collection.immutable.SortedSet

package object flowz {
  type Properties    = Has[Properties.Service]
  type Annotated[+A] = (A, PropertyMap)

  object CommandLineArgs extends Subtype[List[String]]
  type CommandLineArgs = CommandLineArgs.Type

  object Variables extends Subtype[Map[String, String]]
  type Variables = Variables.Type

  type ExecutableFlow[-InitialState, -InputMsg, -R, +E] = Flow[InitialState, Any, InputMsg, R, E, ExitCode]
  type FlowArgs                                         = Has[FlowArguments]
  type FlowBaseEnv                                      = Instrumentor with Clock

//  type ForkedStep[-StateIn, +StateOut, -Env, -Params, +Err, +Output] =
//    Act[StateIn, Unit, Env, Params, Nothing, Fiber.Runtime[Err, StepOutputs[StateOut, Output]]]

  type BehaviorEffect[-SIn, +SOut, -InputMsg, -Env, +E, +A] = ZIO[(SIn, InputMsg, Env), E, BehaviorSuccess[SOut, A]]

  type Activity[-SIn, +SOut, -Msg, -Env, +E, +A] = ZIO[SIn with Msg with Env, E, BehaviorSuccess[SOut, A]]

  type StatelessBehavior[-InputMsg, -R, +E, +A] = Behavior[Any, Any, InputMsg, R, E, A]

  type ZIOBehavior[-R, +E, +A] = Behavior[Any, Any, Any, R, E, A]

  /**
   * A type alias for a behavior that acts like an impure function, taking in an input message
   * (also referred to as input/parameters) and produces a single value, possibly failing
   * with a `Throwable`.
   *
   * For example:
   *
   * {{{
   *   val intConverter:FuncBehavior[String,Int] =
   *    Behavior.fromFunction { numberStr:String => numberStr.toInt }
   * }}}
   */
  type FuncBehavior[-InputMsg, +A] = Behavior[Any, Any, InputMsg, Any, Throwable, A]

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

  def behavior[InitialState, OutputState, InputMsg, R, A](
    f: (InitialState, InputMsg) => URIO[R, BehaviorSuccess[OutputState, A]]
  ): Behavior[InitialState, OutputState, InputMsg, R, Nothing, A] =
    Behavior.behaviorFromFunctionM(f)

  def outputting[OutputState, Value](state: OutputState, value: Value): IndieBehavior[OutputState, Nothing, Value] =
    Behavior.outputting(state, value)

  def process[SIn, SOut, In, R, Err, Out](label: String)(
    children: Flow[SIn, SOut, In, R, Err, Out]*
  ): Flow[SIn, SOut, In, R, Err, Out] =
    Flow.process(label, children = ZManaged.succeed(children.toVector))

  def step[SIn, SOut, In, R, Err, Out](label: String)(
    behavior: Behavior[SIn, SOut, In, R, Err, Out]
  ): Flow[SIn, SOut, In, R, Err, Out] = Flow.step(label, behavior, PropertyMap.empty)

  /**
   * The `Properties` trait provides access to a property map that flows and behaviors
   * can add arbitrary properties to. Each property consists of a string
   * identifier, an initial value, and a function for combining two values.
   * Properties form monoids and you can think of `Properties` as a more
   * structured logging service or as a super polymorphic version of the writer
   * monad effect.
   */
  object Properties {

    trait Service extends Serializable {
      def addProperty[V](key: Property[V], value: V): UIO[Unit]
      def get[V](key: Property[V]): UIO[V]
      def withAnnotation[R, E, A](zio: ZIO[R, E, A]): ZIO[R, Annotated[E], Annotated[A]]
      def supervisedFibers: UIO[SortedSet[Fiber.Runtime[Any, Any]]]
    }

    /**
     * Accesses a `Properties` instance in the environment and appends the
     * specified property to the property map.
     */
    def addProperty[V](key: Property[V], value: V): URIO[Properties, Unit] =
      ZIO.accessM(_.get.addProperty(key, value))

    /**
     * Accesses a `Properties` instance in the environment and retrieves the
     * property of the specified type, or its default value if there is none.
     */
    def get[V](key: Property[V]): URIO[Properties, V] =
      ZIO.accessM(_.get.get(key))

    /**
     * Constructs a new `Properties` service.
     */
    val live: Layer[Nothing, Properties] =
      ZLayer.fromEffect(FiberRef.make(PropertyMap.empty).map { fiberRef =>
        new Properties.Service {
          def addProperty[V](key: Property[V], value: V): UIO[Unit] =
            fiberRef.update(_.annotate(key, value))
          def get[V](key: Property[V]): UIO[V] =
            fiberRef.get.map(_.get(key))
          def withAnnotation[R, E, A](zio: ZIO[R, E, A]): ZIO[R, Annotated[E], Annotated[A]] =
            fiberRef.locally(PropertyMap.empty) {
              zio.foldM(e => fiberRef.get.map((e, _)).flip, a => fiberRef.get.map((a, _)))
            }
          def supervisedFibers: UIO[SortedSet[Fiber.Runtime[Any, Any]]] =
            ZIO.descriptorWith { descriptor =>
              get(Property.fibers).flatMap {
                case Left(_) =>
                  val emptySet = SortedSet.empty[Fiber.Runtime[Any, Any]]
                  ZIO.succeed(emptySet)
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
     * Accesses an `Properties` instance in the environment and executes the
     * specified effect with an empty annotation map, returning the annotation
     * map along with the result of execution.
     */
    def withAnnotation[R <: Properties, E, A](zio: ZIO[R, E, A]): ZIO[R, Annotated[E], Annotated[A]] =
      ZIO.accessM(_.get.withAnnotation(zio))

    /**
     * Returns a set of all fibers in this test.
     */
    def supervisedFibers: ZIO[Properties, Nothing, SortedSet[Fiber.Runtime[Any, Any]]] =
      ZIO.accessM(_.get.supervisedFibers)
  }

  /**
   * A `FlowReporter[E]` is capable of reporting flow execution results with error type `E`.
   */
  type FlowReporter[-E] = (Duration, ExecutedFlow[E]) => URIO[Instrumentor, Unit]
  object FlowReporter {

    /**
     * A `FlowReporter` that does nothing.
     */
    val silent: FlowReporter[Any] = (_, _) => ZIO.unit
  }
}
