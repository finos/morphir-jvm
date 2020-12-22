package morphir.flowz.spark.testing

import morphir.flowz.spark.sparkModule.SparkModule
import morphir.flowz.spark.testing.SparkSpec.SparkTestingEnv
import org.apache.spark.SparkConf
import org.apache.spark.sql.SparkSession
import zio.duration.durationInt
import zio.logging._
import zio.logging.slf4j.Slf4jLogger
import zio.test.{ RunnableSpec, TestAspect, TestExecutor, TestRunner }
import zio.test.environment.{ TestEnvironment, testEnvironment }
import zio.{ ULayer, ZLayer }

abstract class SparkSpec extends RunnableSpec[SparkTestingEnv, Any] {
  val sparkSessionBuilder: SparkSession.Builder = SparkSpec.sparkSessionBuilder

  val sparkTestingLayer: ULayer[SparkTestingEnv] = {
    val logFormat = "[correlation-id = %s] %s"
    val logging = Slf4jLogger.make { (context, message) =>
      val correlationId = LogAnnotation.CorrelationId.render(
        context.get(LogAnnotation.CorrelationId)
      )
      logFormat.format(correlationId, message)
    }

    val spark =
      SparkModule.buildLayer(sparkSessionBuilder, newSession = true)
    ZLayer.succeed(sparkSessionBuilder) >>> SparkModule.fromSparkSessionBuilder

    testEnvironment ++ logging ++ spark
  }.orDie

  override def aspects: List[TestAspect[Nothing, SparkTestingEnv, Nothing, Any]] =
    List(TestAspect.timeout(60.seconds))

  override def runner: TestRunner[SparkTestingEnv, Any] =
    TestRunner(TestExecutor.default(sparkTestingLayer))
}
object SparkSpec {
  val sparkSessionBuilder: SparkSession.Builder = {
    val sparkConf = new SparkConf()
      .setMaster("local[*]")
      .setAppName("spark-spec")
      .set("spark.ui.enabled", "false")
      .set("spark.driver.host", "localhost")
    SparkSession.builder().config(sparkConf)
  }
  type SparkTestingEnv = TestEnvironment with Logging with SparkModule

}
