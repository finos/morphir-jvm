package org.finos.morphir.ir
import scala.collection.immutable.ListMap

/**
 * Constructors in a dictionary keyed by their name.
 * The values are the argument types for each constructor
 */
final case class TypeConstructors[+A](lookup: ListMap[Name, List[(Name, Type[A])]])
