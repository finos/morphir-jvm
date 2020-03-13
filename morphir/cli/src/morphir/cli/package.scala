package morphir

import java.nio.file.{Path => JPath}

import zio._
import zio.blocking.Blocking
import zio.console.Console
import zio.stream.ZStream

package object cli {

  type Cli = Has[Cli.Service]

  def generateScala(modelPath: JPath, output: JPath) =
    ZIO.accessM[Cli with morphir.sdk.ModelLoader with Blocking with Console](
      _.get.generateScala(modelPath, output)
    )

  def elmMake(
      projectDir: Option[JPath] = None,
      output: Option[JPath] = None,
      help: Boolean = false
  ) =
    ZStream.accessStream[Cli with Blocking](
      _.get.elmMake(projectDir, output, help)
    )
}
