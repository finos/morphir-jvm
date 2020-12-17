package morphir.flowz

import morphir.flowz.spark.sparkModule.SparkModule

package object spark {
  type SparkStep[-StateIn, +StateOut, -Env, -Params, +Err, +Value] =
    Step[StateIn, StateOut, Env with SparkModule, Params, Err, Value]

  object api extends SparkApi
}
