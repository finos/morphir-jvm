package org.morphir.toolbox.workspace.services.files

import java.nio.file.{Path, Paths}

import io.github.soc.directories.UserDirectories

private[files] object FilesModule {

  def userHomeDir: Path = Paths.get(UserDirectories.get().homeDir)

}
