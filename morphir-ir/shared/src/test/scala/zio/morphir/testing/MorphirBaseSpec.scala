package zio.morphir.testing

import zio.*
import zio.test.{DefaultRunnableSpec, TestAspect}

abstract class MorphirBaseSpec extends DefaultRunnableSpec {
  override def aspects = List(TestAspect.timeout(60.seconds))
}
