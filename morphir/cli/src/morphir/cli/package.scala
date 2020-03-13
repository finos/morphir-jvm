package morphir

import java.nio.file.{Path => JPath}

import zio._
import zio.blocking.Blocking
import zio.stream.ZStream

package object cli {

  type Cli = Has[Cli.Service]

  def elmMake(
      projectDir: Option[JPath] = None,
      output: Option[JPath] = None,
      help: Boolean = false
  ) =
    ZStream.accessStream[Cli with Blocking](
      _.get.elmMake(projectDir, output, help)
    )
}
