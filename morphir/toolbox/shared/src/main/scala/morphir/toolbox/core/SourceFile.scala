package morphir.toolbox.core

import java.nio.file.Path

case class SourceFile[A](path: Path, data: A)
