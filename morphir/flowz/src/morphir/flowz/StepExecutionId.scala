package morphir.flowz

import zio.URIO

object StepUid {
  def nextUid: URIO[StepUidGenerator, StepUid] =
    uidGenerator.nextUid[Step.type]
}
