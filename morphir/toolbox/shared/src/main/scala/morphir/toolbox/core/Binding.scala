package morphir.toolbox.core

import java.nio.file.Path

case class Binding(name: String, srcDirs: List[Path], outDir: Path)
