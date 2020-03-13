package morphir.sdk

import java.io.IOException
import java.nio.file.{Path => JPath}
import java.nio.file.{Path => JPath}

import upickle.default._
import ujson.Readable

import zio._
import zio.blocking.Blocking
import zio.nio.file.Files
import zio.nio.core.file.Path

trait SdkModules {

  type ModelLoader = Has[ModelLoader.Service]

  object modelLoader {

    def loadJsonFromFile(
        path: Path
    ): ZIO[ModelLoader with Blocking, IOException, ujson.Value] =
      ZIO.accessM(_.get.loadJsonFromFile(path))

    def loadJsonFromFile(
        path: JPath
    ): ZIO[ModelLoader with Blocking, IOException, ujson.Value] =
      ZIO.accessM(_.get.loadJsonFromFile(path))
  }

}
