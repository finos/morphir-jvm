package morphir.flowz

import morphir.flowz.uidGenerator.UidGenerator
import zio.ZLayer
import zio.clock.Clock

object StepUidGenerator {
  val live: ZLayer[Clock, Nothing, StepUidGenerator] = UidGenerator.live
}
