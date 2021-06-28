package org.finos.morphir.ir
import scala.collection.immutable.ListMap

/**
 * Type that represents a module definition. A module definition contains all the details
 * including implementation and private types and values.
 * 
 * A module contains types and values which is represented by two field in this type:
 *   - types: a map of local name to access controlled, documented type specification.
 *   - values: a map of local name to access controlled value specification.
 */
final case class ModuleDefinition[+TA,+VA](
    types: ListMap[Name, AccessControlled[Documented[TypeDefinition[TA]]]],
    values: ListMap[Name, AccessControlled[ValueDefinition[TA,VA]]]):

    def lookupValueDefinition(name:Name): Option[ValueDefinition[TA,VA]] =
        values get(name) map (_.withPrivateAccess)

object ModuleDefinition:
    def empty[TA,VA]:ModuleDefinition[TA,VA] = ModuleDefinition[TA,VA](ListMap.empty, ListMap.empty)
