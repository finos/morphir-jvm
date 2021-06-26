package org.finos.morphir.ir

/**
 * Type that represents a value or function specification.
 * The specification of what the value or function is without the actual
 * data or logic behind it
 */
final case class Specification[+A](inputs: List[(Name, Type[A])], output: Type[A]):
  def transform[B](f:A => B):Specification[B] =
      Specification(
          inputs = inputs.map { case (name, tpe) => (name, tpe.transform(f))},
          output = output.transform(f)
      )

final case class TypeSpecification[+A](typeParams:List[Name], details:TypeSpecificationDetails[A])

enum TypeSpecificationDetails[+A]:
    self =>
    case CustomType(ctors:TypeConstructors[A])
    case OpaqueType
    case TypeAlias(typeExp:Type[A])

    def transform[B](f:A=>B):TypeSpecificationDetails[B] = self match 
        case CustomType(ctors) => CustomType(ctors.transform(f))
        case OpaqueType => OpaqueType
        case TypeAlias(typeExp) => TypeAlias(typeExp.transform(f))


