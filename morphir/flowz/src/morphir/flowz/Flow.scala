package morphir.flowz
import zio._

sealed trait Flow[-Imports, -Inputs, +Err, +Outputs] {}

object Flow {}
