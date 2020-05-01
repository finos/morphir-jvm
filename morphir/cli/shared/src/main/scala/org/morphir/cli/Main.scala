package org.morphir.cli

import org.morphir.toolbox.workspace.WorkspaceModule
import zio._

object Main extends App {

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (for {
      cmd      <- ZIO.fromEither(Cli.rootCommand.parse(args))
      exitCode <- cmd.execute
      ec       = exitCode.code
    } yield ec)
      .provideSomeLayer[zio.ZEnv](WorkspaceModule.live)
      .catchAll(help => ZIO.effectTotal(System.err.println(help)) *> ZIO.succeed(1))

}
