package org.morphir.toolbox.workspace.config

import org.morphir.toolbox.core.ProjectPath

case class ProjectSettings(
  name: Option[String] = None,
  projectDir: Option[ProjectPath] = None,
  bindings: BindingsSettings = BindingsSettings.default
)
