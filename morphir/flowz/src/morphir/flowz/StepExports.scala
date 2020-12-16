package morphir.flowz

trait StepExports {
  def stage[StateIn, StateOut, Env, Params, Err, Out](
    func: (StateIn, Params) => Step[StateIn, StateOut, Env, Params, Err, Out]
  ): Step[StateIn, StateOut, Env, Params, Err, Out] = Step.stage(func)

  def step[StateIn, StateOut, Params, Out](
    func: (StateIn, Params) => (StateOut, Out)
  ): Step[StateIn, StateOut, Any, Params, Throwable, Out] = Step.step(func)
}
