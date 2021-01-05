package morphir.flowz

import zio.Fiber

trait StepExports {
  type ForkedStep[-StateIn, +StateOut, -Env, -Params, +Err, +Output] =
    Step[StateIn, Unit, Env, Params, Nothing, Fiber.Runtime[Err, StepOutputs[StateOut, Output]]]

  final type Activity[-Env, -Params, +Err, +Value] = Step[Any, Value, Env, Params, Err, Value]

  def stage[StateIn, StateOut, Env, Params, Err, Out](
    func: (StateIn, Params) => Step[StateIn, StateOut, Env, Params, Err, Out]
  ): Step[StateIn, StateOut, Env, Params, Err, Out] = Step.stage(func)

  def step[StateIn, StateOut, Params, Out](
    func: (StateIn, Params) => (StateOut, Out)
  ): Step[StateIn, StateOut, Any, Params, Throwable, Out] = Step.step(func)

  final type Step[-StateIn, +StateOut, -Env, -Params, +Err, +Value] =
    morphir.flowz.Step[StateIn, StateOut, Env, Params, Err, Value]
  val Step: morphir.flowz.Step.type = morphir.flowz.Step

  final type StepOutputs[+State, +Value] = morphir.flowz.StepOutputs[State, Value]
  val StepOutputs: morphir.flowz.StepOutputs.type = morphir.flowz.StepOutputs

  final type StepInputs[+State, +Params] = morphir.flowz.StepInputs[State, Params]
  val StepInputs: morphir.flowz.StepInputs.type = morphir.flowz.StepInputs

  final type StepContext[+Env, +State, +Params] = morphir.flowz.StepContext[Env, State, Params]
  val StepContext: morphir.flowz.StepContext.type = morphir.flowz.StepContext
}
