package morphir.flowz

import zio.test.Assertion._
import zio.test.{ DefaultRunnableSpec, ZSpec, assert }
import zio.ZIO

object AnnotationsSpec extends DefaultRunnableSpec {
  def spec: ZSpec[Environment, Failure] = suite("annotationsSpec")(
    testM("withAnnotation executes specified effect with an empty annotation map") {
      for {
        _   <- Annotations.annotate(count, 1)
        a   <- Annotations.get(count)
        map <- Annotations.withAnnotation(ZIO.unit <* Annotations.annotate(count, 2)).map(_._2)
        b    = map.get(count)
      } yield assert(a)(equalTo(1)) && assert(b)(equalTo(2))
    },
    testM("withAnnotation returns annotation map with result") {
      for {
        map <- Annotations.withAnnotation(Annotations.annotate(count, 3) *> ZIO.fail("fail")).flip.map(_._2)
        c    = map.get(count)
      } yield assert(c)(equalTo(3))
    }
  ).provideCustomLayer(Annotations.live)

  val count: StepAnnotation[Int] = StepAnnotation[Int]("count", 0, _ + _)
}
