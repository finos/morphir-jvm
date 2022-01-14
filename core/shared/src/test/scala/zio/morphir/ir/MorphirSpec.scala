package zio.morphir.ir

import zio.test.DefaultRunnableSpec
import zio.test.TestAspect
import zio.duration.*

abstract class MorphirBaseSpec extends DefaultRunnableSpec {
  override def aspects = List(TestAspect.timeout(60.seconds))
}