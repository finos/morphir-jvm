package morphir.sdk

import java.io.IOException
import java.nio.file.{Path => JPath}
import java.nio.file.{Path => JPath}

import upickle.default._
import ujson.Readable
import java.nio.file.{Path => JPath}

import zio._
import zio.blocking.Blocking
import morphir.internal.io.file.Files._

trait SdkModules {

  type ModelLoader = Has[ModelLoader.Service]

  object modelLoader {

    def loadJsonFromFile(
        path: JPath
    ): ZIO[ModelLoader with Blocking, IOException, ujson.Value] =
      ZIO.accessM(_.get.loadJsonFromFile(path))
  }

}
