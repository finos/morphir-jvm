package morphir.flowz

import zio.clock.Clock
import zio.console.Console
import zio.logging.{ Logger, Logging }
import zio.{ Has, UIO, URIO, ZIO, ZLayer }

object instrumentation {
  type Instrumentation = Has[Instrumentation.Service]

  val logger: URIO[Instrumentation, Logger[String]] =
    ZIO.accessM(_.get.logger)

  object Instrumentation {
    trait Service {
      def logger: UIO[Logger[String]]
    }
  }

  val default: ZLayer[Console with Clock, Nothing, Instrumentation] = Logging
    .console()
    .map(theLogging =>
      Has(new Instrumentation.Service {
        val logger: UIO[Logger[String]] = ZIO.succeed(theLogging.get)
      })
    )
}
