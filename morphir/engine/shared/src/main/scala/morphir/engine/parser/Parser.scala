package morphir.engine.parser

import morphir.io.file.VFile
import zio._
import zio.stream._

object Parser {
  trait Service {
    def parseFiles(files: Chunk[VFile[Any]]): Stream[ParserError, VFile[Any]]
  }

}
