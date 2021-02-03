package morphir.flowz

import zio.Has

final case class FlowRunner[R <: Has[_], E](executor: FlowExecutor[R, E]) {}
