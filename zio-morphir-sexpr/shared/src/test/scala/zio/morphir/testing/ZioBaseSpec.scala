package zio.morphir.testing

import zio.*
import zio.test.*

trait ZioBaseSpec extends DefaultRunnableSpec {
  override def aspects = List(TestAspect.timeout(60.seconds))
}
