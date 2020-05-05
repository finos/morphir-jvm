package org.morphir.toolbox.core

import java.nio.file.Path

case class Project(
  name: ProjectName,
  projectDir: ProjectPath,
  bindings: Bindings,
  targets: List[Target[Any]],
  sources: List[SourceFile[Any]]
) {}

object Project {}

case class Artifact[A](path: Path, manifest: ArtifactManifest, data: A)
case class ArtifactManifest()
