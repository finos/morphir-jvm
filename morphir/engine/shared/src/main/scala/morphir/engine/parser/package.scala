package morphir.engine

import morphir.io.file.VFile
import zio._
import zio.stream._

package object parser {
  type Parser = Has[Parser.Service]

  def parseFiles(files: Chunk[VFile]): ZStream[Parser, ParserError, VFile] =
    ZStream.accessStream[Parser](_.get.parseFiles(files))
}
