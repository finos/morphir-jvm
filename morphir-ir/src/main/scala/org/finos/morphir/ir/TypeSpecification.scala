package org.finos.morphir.ir

final case class TypeSpecification[+A](typeParams: List[Name], details: TypeSpecificationDetails[A])

enum TypeSpecificationDetails[+A]:
  self =>
  case CustomType(ctors: TypeConstructors[A])
  case OpaqueType
  case TypeAlias(typeExp: Type[A])

  def transform[B](f: A => B): TypeSpecificationDetails[B] = self match
    case CustomType(ctors)  => CustomType(ctors.transform(f))
    case OpaqueType         => OpaqueType
    case TypeAlias(typeExp) => TypeAlias(typeExp.transform(f))
