package zio.morphir.testing

import zio.*
import zio.test.*

trait ZioBaseSpec extends ZIOSpecDefault {
  override def aspects = Chunk(TestAspect.timeout(60.seconds))
}
