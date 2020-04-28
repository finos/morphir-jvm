package org.morphir.toolbox.workspace.config

import java.nio.file.Path

case class BindingSettings(srcDirs: List[Path], outDir: Path)

object BindingSettings {
  object defaults {
    val elm: BindingSettings = BindingSettings(
      srcDirs = List(Path.of("src/elm/")),
      outDir = Path.of("out/")
    )
  }
}
