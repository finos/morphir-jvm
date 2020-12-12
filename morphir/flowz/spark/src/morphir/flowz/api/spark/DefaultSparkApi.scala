package morphir.flowz.api.spark

import morphir.flowz.Api
import morphir.flowz.spark.SparkApi

trait DefaultSparkApi extends SparkApi {
  override val flowzApi: Api = morphir.flowz.default
}
