package org.morphir

import zio._

package object cli {
  type CliEnv    = Main.CliEnv
  type CliAction = RIO[CliEnv, ExitCode]
}
