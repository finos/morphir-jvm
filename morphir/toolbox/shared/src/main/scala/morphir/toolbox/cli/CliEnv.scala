package morphir.toolbox.cli

import morphir.toolbox.workspace.WorkspaceModule
import zio.blocking.Blocking
import zio.clock.Clock
import zio.console.Console
import zio.random.Random
import zio.{ system, ZLayer }

object CliEnv {
  val live
    : ZLayer[Any, Throwable, Clock with Console with system.System with Random with WorkspaceModule with Blocking] =
    Clock.live ++
      Console.live ++
      system.System.live ++
      Random.live ++
      (Blocking.live >>> WorkspaceModule.live) ++ Blocking.live

}
