package morphir.toolbox.workspace.services.files

import java.nio.file.{ Path, Paths }

private[files] object FilesModule {

  def userHomeDir: Path = Paths.get(sys.props.get("user.home").get)

}
