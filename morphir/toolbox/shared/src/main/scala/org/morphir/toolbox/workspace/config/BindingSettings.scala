package org.morphir.toolbox.workspace.config

import java.nio.file.{ Path, Paths }

case class BindingSettings(srcDirs: List[Path], outDir: Path)

object BindingSettings {
  object defaults {
    val elm: BindingSettings = BindingSettings(
      srcDirs = List(Paths.get("src/elm/")),
      outDir = Paths.get("out/")
    )
  }
}
