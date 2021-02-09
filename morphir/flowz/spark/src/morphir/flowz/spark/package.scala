package morphir.flowz

import morphir.flowz.spark.sparkModule.SparkModule

package object spark {
  type SparkBehavior[-StateIn, +StateOut, -Env, -Params, +Err, +Value] =
    Behavior[StateIn, StateOut, Env with SparkModule, Params, Err, Value]

  object api extends SparkApi
}
