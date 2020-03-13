package morphir.sdk

import java.io.IOException
import java.nio.file.{Path => JPath}

import upickle.default._
import ujson.Readable

import zio._
import zio.blocking.Blocking
import zio.nio.file.Files
import zio.nio.core.file.Path

object ModelLoader {
  trait Service {
    def loadJsonFromFile(path: Path): ZIO[Blocking, IOException, ujson.Value]
    def loadJsonFromFile(path: JPath): ZIO[Blocking, IOException, ujson.Value] =
      loadJsonFromFile(Path.fromJava(path))
  }

  val live = ZLayer.succeed(
    new Service {
      def loadJsonFromFile(path: Path) =
        Files
          .readAllBytes(path)
          .map(chunks =>
            read[ujson.Value](Readable.fromByteArray(chunks.toArray))
          )
    }
  )

}
