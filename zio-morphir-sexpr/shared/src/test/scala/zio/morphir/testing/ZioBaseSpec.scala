package zio.morphir.testing

import zio.duration._
import zio.test._
import zio.test.environment.Live

trait ZioBaseSpec extends DefaultRunnableSpec {
  override def aspects: List[TestAspectAtLeastR[Live]] = List(TestAspect.timeout(60.seconds))
}
