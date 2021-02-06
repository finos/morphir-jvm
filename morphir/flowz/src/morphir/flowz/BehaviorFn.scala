package morphir.flowz

import zio.ZIO

private[flowz] abstract class BehaviorFn[-SIn, +SOut, -Msg, -R, +E, +A]
    extends ((SIn, Msg) => ZIO[R, E, (SOut, A)])
    with Serializable {
  def underlying: AnyRef
}
