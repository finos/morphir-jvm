package morphir.flowz

import morphir.flowz.spark.sparkModule.SparkModule

package object spark {
  type SparkFlow[-StateIn, +StateOut, -Env, -Input, +Err, +Output] =
    Flow[StateIn, StateOut, Env with SparkModule, Input, Err, Output]

  type SparkStep[-Env, -In, +Err, +Out] = Step[Env with SparkModule, In, Err, Out]
}
