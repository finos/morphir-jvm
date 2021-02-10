package morphir.flowz

import zio.URIO

object StepExecutionId {
  def nextExecutionId: URIO[StepUidGenerator, StepExecutionId] =
    uidGenerator.nextUid[Step.type]
}
