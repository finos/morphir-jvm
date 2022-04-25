package org.finos.morphir.ir

import scala.collection.immutable.ListMap

final case class PackageDefinition[TA, VA](
    modules: ListMap[ModuleName, AccessControlled[ModuleDefinition[TA, VA]]]
  )
object PackageDefinition:
  val empty: PackageDefinition[Unit, Unit] = PackageDefinition[Unit, Unit](ListMap.empty)
