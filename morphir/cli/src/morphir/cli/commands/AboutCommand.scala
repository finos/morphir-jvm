package morphir.cli.commands

import zio._
import zio.console._
import zio.clock.Clock
import morphir.cli._
import morphir.runtime._
import caseapp.core.RemainingArgs
import morphir.gateway.ZioGateway.GatewayClient
import scalapb.zio_grpc.`package`.ZManagedChannel
import io.grpc.ManagedChannelBuilder
import morphir.gateway.AboutRequest
import org.scalacheck.util.CmdLineParser

object AboutCommand extends MorphirCommand[CliCommand.About] {
  def run(commandLine: CliCommand.About, remainingArgs: RemainingArgs): Cmdlet =
    (for {
      _ <- console.putStrLn("Running about command.")
      exitCode <- logic
    } yield exitCode)

  val clientLayer: ZLayer.NoDeps[Throwable, GatewayClient] = GatewayClient.live(
    ZManagedChannel(
      ManagedChannelBuilder.forAddress("localhost", 7779).usePlaintext()
    )
  )

  val logic: Cmdlet =
    (for {
      _ <- putStrLn(s"Starting client...")
      r <- GatewayClient.about(AboutRequest()).provideLayer(clientLayer)
      _ <- putStrLn(s"$r")
    } yield ExitCode.Success)
      .onError { c => putStrLn(c.prettyPrint) }
      .fold(_ => ExitCode.Failure, c => c)
}
