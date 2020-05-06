package morphir.engine

import morphir.io.file.VirtualFile
import zio._
import zio.stream._

package object parser {
  type Parser = Has[Parser.Service]

  def parseFiles(files: Chunk[VirtualFile]): ZStream[Parser, ParserError, VirtualFile] =
    ZStream.accessStream[Parser](_.get.parseFiles(files))
}
