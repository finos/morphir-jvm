package morphir.flowz

import zio.test.Assertion._
import zio.test.{ DefaultRunnableSpec, ZSpec, assert }
import zio.ZIO

object AnnotationsSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] = suite("annotationsSpec")(
    testM("withProperty executes specified effect with an empty annotation map") {
      for {
        _   <- Properties.addProperty(count, 1)
        a   <- Properties.get(count)
        map <- Properties.withProperty(ZIO.unit <* Properties.addProperty(count, 2)).map(_._2)
        b    = map.get(count)
      } yield assert(a)(equalTo(1)) && assert(b)(equalTo(2))
    },
    testM("withProperty returns annotation map with result") {
      for {
        map <- Properties.withProperty(Properties.addProperty(count, 3) *> ZIO.fail("fail")).flip.map(_._2)
        c    = map.get(count)
      } yield assert(c)(equalTo(3))
    }
  ).provideCustomLayer(Properties.live)

  val count: Property[Int] = Property[Int]("count", 0, _ + _)
}
