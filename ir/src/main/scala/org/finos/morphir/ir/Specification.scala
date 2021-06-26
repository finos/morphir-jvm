package org.finos.morphir.ir

/**
 * Type that represents a value or function specification.
 * The specification of what the value or function is without the actual
 * data or logic behind it
 */
final case class Specification[+A](inputs: List[(Name, Type[A])], output: Type[A])
  // def mapAttributes[B](f:A => B):Specification[B] =
  //     Specification(inputs = inputs.map{
  //         case (name, tpe) => (name, tpe.)
  //     })

final case class TypeSpecification[+A](typeParams:List[Name], details:TypeSpecificationDetails[A])

enum TypeSpecificationDetails[+A]:
    case TypeAlias(typeExp:Type[A])
    case OpaqueType
    case CustomType(ctors:TypeConstructors[A])


