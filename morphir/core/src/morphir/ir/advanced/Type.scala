package morphir.ir.advanced
import morphir.ir.{FQName, Name}

sealed trait Type[+Extra]
object Type {
  case class Variable[A](name: Name, extra: A) extends Type[A]
  case class Reference[A](
      typeName: FQName,
      typeParameters: List[Type[A]],
      extra: A
  ) extends Type[A]

  case class Tuple[A](elementTypes: List[Type[A]], extra: A) extends Type[A]
  case class Record[A](fieldTypes: List[Field[A]], extra: A) extends Type[A]
  case class ExtensibleRecord[A]() extends Type[A]
  case class Function[A](argType: Type[A], returnType: Type[A], extra: A)
      extends Type[A]
  case class Unit[A](extra: A) extends Type[A]

  case class Field[A](name: Name, fieldType: Type[A])

  def unit[X](extra: X): Type[X] = Unit[X](extra)

  sealed abstract class Declaration[+X]
  object Declaration {
    case class TypeAliasDeclaration[+X](
        typeParams: List[Name],
        typeExp: Declaration[X]
    ) extends Declaration[X]

    def typeAliasDeclaration[X](
        typeParams: List[Name],
        typeExp: Declaration[X]
    ): Declaration[X] = TypeAliasDeclaration[X](typeParams, typeExp)

    case class OpaqueTypeDeclaration[+X](typeParams: List[Name])
        extends Declaration[X]

    def opaqueTypeDeclaration[X](typeParams: List[Name]): Declaration[X] =
      OpaqueTypeDeclaration(typeParams)

    case class CustomTypeDeclaration[+X](
        typeParams: List[Name],
        ctors: Declaration[X]
    ) extends Declaration[X]

    def customTypeDeclaration[X](
        typeParams: List[Name],
        ctors: Declaration[X]
    ): Declaration[X] =
      CustomTypeDeclaration(typeParams, ctors)
  }

  sealed abstract class Definition[+X]
  object Definition {
    case class TypeAliasDefinition[X](typeParams: List[Name], typeExp: Type[X])
        extends Definition[X]

    case class CustomTypeDefinition[X](typeParams: List[Name], typeExp: Type[X])
        extends Definition[X]
  }
}
