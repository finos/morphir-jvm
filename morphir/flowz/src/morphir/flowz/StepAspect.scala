package morphir.flowz

trait StepAspect[-SIn, +SOut, -Msg, -R, +E, +A] {
  def apply[SIn1 <: SIn, SOut1 >: SOut, Msg1 <: Msg, R1 <: R, E1 >: E, A1 >: A](
    step: Step[SIn1, SOut1, Msg1, R1, E1, A1]
  ): Step[SIn1, SOut1, Msg1, R1, E1, A1]
}

object StepAspect {
//  val traced: StepAspect[Any, Nothing, Any, InstrumentationLogging with StepUidGenerator, Nothing, Nothing] =
//    new StepAspect[Any, Nothing, Any, InstrumentationLogging, Nothing, Nothing] {
//      def apply[
//        SIn1 <: Any,
//        SOut1 >: Nothing,
//        Msg1 <: Any,
//        R1 <: InstrumentationLogging with StepUidGenerator,
//        E1 >: Nothing,
//        A1 >: Nothing
//      ](
//        step: RunnableStep[SIn1, SOut1, Msg1, R1, E1, A1]
//      ): RunnableStep[SIn1, SOut1, Msg1, R1, E1, A1] =
//        Step(for {
//
//          _   <- iLog.trace(InstrumentationEvent.stepExecutionStarted())
//          res <- step.asEffect
//        } yield res)
//    }
}
