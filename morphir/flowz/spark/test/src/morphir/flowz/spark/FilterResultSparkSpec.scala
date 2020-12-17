package morphir.flowz.spark

import morphir.flowz.FilterResult
import morphir.flowz.spark.testing.SparkSpec
import zio.ZIO
import zio.test._
import zio.test.Assertion._

object FilterResultSparkSpec extends SparkSpec {
  def spec = suite("FilterResult Spark Spec")(
    testM("It should support encoding into a Spark dataset")(
      for {
        data    <- ZIO.succeed(List(FilterResult.included("A"), FilterResult.excluded("B"), FilterResult.included("C")))
        dataset <- sparkModule.createDataset(data)
        _       <- ZIO.effect(dataset.show(false))
        actual  <- ZIO.effect(dataset.collect().toList)
      } yield assert(actual)(equalTo(data))
    )
  )
}
