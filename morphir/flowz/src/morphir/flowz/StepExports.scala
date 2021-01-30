package morphir.flowz

import zio.Fiber

trait StepExports {
  def stage[StateIn, StateOut, Env, Params, Err, Out](
    func: (StateIn, Params) => Step[StateIn, StateOut, Env, Params, Err, Out]
  ): Step[StateIn, StateOut, Env, Params, Err, Out] = Step.stage(func)

  def step[StateIn, StateOut, Params, Out](
    func: (StateIn, Params) => (StateOut, Out)
  ): Step[StateIn, StateOut, Any, Params, Throwable, Out] = Step.step(func)

  final type Activity[-Env, -Params, +Err, +Value] = Step[Any, Value, Env, Params, Err, Value]
  val Activity: morphir.flowz.Activity.type = morphir.flowz.Activity

  type ForkedStep[-StateIn, +StateOut, -Env, -Params, +Err, +Output] =
    Step[StateIn, Unit, Env, Params, Nothing, Fiber.Runtime[Err, StepOutputs[StateOut, Output]]]

  final type Step[-StateIn, +StateOut, -Env, -Params, +Err, +Value] =
    morphir.flowz.Act[StateIn, StateOut, Env, Params, Err, Value]
  val Step: morphir.flowz.Act.type = morphir.flowz.Act

  final type IOStep[-Params, +Err, +Value] = Step[Any, Unit, Any, Params, Err, Value]
  final type TaskStep[-Params, +Value]     = Step[Any, Unit, Any, Params, Throwable, Value]
  final type UStep[-Params, +Value]        = Step[Any, Unit, Any, Params, Nothing, Value]

  final type StepOutputs[+State, +Value] = morphir.flowz.StepOutputs[State, Value]
  val StepOutputs: morphir.flowz.StepOutputs.type = morphir.flowz.StepOutputs

  final type StepInputs[+State, +Params] = morphir.flowz.StepInputs[State, Params]
  val StepInputs: morphir.flowz.StepInputs.type = morphir.flowz.StepInputs

  final type ActContext[+Env, +State, +Params] = morphir.flowz.ActContext[Env, State, Params]
  val ActContext: morphir.flowz.ActContext.type = morphir.flowz.ActContext
}
