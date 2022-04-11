package zio.morphir.testing

import zio.*
import zio.test.{TestAspect, ZIOSpecDefault}

abstract class MorphirBaseSpec extends ZIOSpecDefault {
  override def aspects = Chunk(TestAspect.timeout(60.seconds))
}
