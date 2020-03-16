package morphir.sdk

import java.io.IOException
import java.nio.file.{Path => JPath}

import upickle.default._
import ujson.Readable

import zio._
import zio.blocking.Blocking
import morphir.internal.io.file.Files._

object ModelLoader {
  trait Service {
    def loadJsonFromFile(path: JPath): ZIO[Blocking, IOException, ujson.Value]
  }

  val live = ZLayer.succeed(
    new Service {
      def loadJsonFromFile(path: JPath) =
        readAllBytes(path)
          .map(chunks =>
            read[ujson.Value](Readable.fromByteArray(chunks.toArray))
          )
    }
  )

}
