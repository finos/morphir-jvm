package morphir.engine.parser.elm

import morphir.engine.parser.{ Parser, ParserError }
import morphir.io.file.VFile
import zio.{ stream, Chunk }

case class ElmParser() extends Parser.Service {
  override def parseFiles(files: Chunk[VFile[Any]]): stream.Stream[ParserError, VFile[Any]] = ???
}
