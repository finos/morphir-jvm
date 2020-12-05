package morphir.flowz.spark

import zio._

object sparkReader {
  type SparkReader = Has[SparkReader.Service]
  object SparkReader {
    trait Service extends Serializable {}

    object Service {}
  }
}
