package morphir.flowz

import zio.{ Has, Layer, UIO }

abstract class FlowExecutor[+R <: Has[_], E] {
  def run(flow: Flow[Any, Any, Any, R, E, Unit]): UIO[ExecutedFlow[E]]
  def environment: Layer[Nothing, R]
}
