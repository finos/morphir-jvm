package org.morphir.toolbox

import org.morphir.toolbox.workspace.WorkspaceModule
import zio.blocking.Blocking
import zio.clock.Clock
import zio.console.Console
import zio.random.Random
import zio.system.System

package object cli {
  type CliEnv = Clock with Console with System with Random with Blocking with WorkspaceModule
}
