package org.morphir.toolbox.workspace.config
import org.morphir.toolbox.core.codecs.TomlSupport
import toml.Codecs._
import toml._

case class WorkspaceSettings(projects: Map[String, ProjectSettings])

object WorkspaceSettings extends TomlSupport {
  def default(): WorkspaceSettings = WorkspaceSettings(Map.empty)

  def fromToml(text: String): Either[toml.Parse.Error, WorkspaceSettings] =
    Toml.parseAs[WorkspaceSettings](text)

}
