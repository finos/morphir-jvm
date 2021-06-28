package org.finos.morphir.ir

final case class TypeDefinition[+A](typeParams:List[Name], details:TypeDefinitionDetails[A])

enum TypeDefinitionDetails[+A]:
    case TypeAlias(typeParams:List[Name], ref:Type[A])
    case CustomType(typeParams:List[Name], ctors:AccessControlled[TypeConstructors[A]])

