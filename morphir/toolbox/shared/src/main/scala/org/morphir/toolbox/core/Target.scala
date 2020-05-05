package org.morphir.toolbox.core

case class Target[A](name: String, artifacts: List[Artifact[A]])
