package morphir

import zio.nio.core.file.Path
import zio.nio.file.Files

object Loader {
  def contentsFrom(path: Path) =
    Files.readAllLines(path)
}
