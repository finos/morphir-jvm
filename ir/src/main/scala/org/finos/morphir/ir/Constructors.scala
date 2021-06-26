package org.finos.morphir.ir
import scala.collection.immutable.ListMap

/**
 * Constructors in a dictionary keyed by their name.
 * The values are the argument types for each constructor
 */
final case class TypeConstructors[+A](lookup: ListMap[Name, List[(Name, Type[A])]]):
    def transform[B](f:A => B):TypeConstructors[B] =
        TypeConstructors(
            lookup.map( (name, args) => 
                (name, args.map((argName,argType) => (argName, argType.transform(f))))
            )
        )
