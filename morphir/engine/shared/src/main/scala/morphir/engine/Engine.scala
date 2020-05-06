package morphir.engine

import morphir.io.file.VFile
import zio.stream._

sealed abstract class Engine {
  def process(files: Iterable[VFile[Any]]): Stream[Throwable, VFile[Any]]
}

object Engine {}
