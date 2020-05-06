package morphir.toolbox.workspace.config

import morphir.toolbox.core.ProjectPath

case class ProjectSettings(
  name: Option[String] = None,
  projectDir: Option[ProjectPath] = None,
  bindings: BindingsSettings = BindingsSettings.default
)
