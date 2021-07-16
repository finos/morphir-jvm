package org.finos.morphir.ir

case class FQName(
    packagePath: Path,
    modulePath: Path,
    localName: Name
  )
