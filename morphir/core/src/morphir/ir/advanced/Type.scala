package morphir.ir.advanced
import morphir.ir.{FQName, Name}
import upickle.default.{readwriter, ReadWriter => RW, macroRW}
import upickle.default._
import morphir.ir.advanced.Type.Declaration.CustomTypeDeclaration
import morphir.sdk.json.{Decode => JD, Encode => JE}

sealed abstract class Type[Extra] {
  def jsonEncode(implicit extraEncoder: Writer[Extra]): ujson.Value = ujson.Null
}

object Type {
  val typeKey = "@type"

  def unit[X](extra: X): Type[X] = Unit[X](extra)

  def typeAliasDeclaration[X](
      typeParams: List[Name]
  )(typeExp: Type[X]): Declaration[X] =
    Declaration.TypeAliasDeclaration(typeParams, typeExp)

  def typeAliasDeclaration[X](
      typeParams: List[Name]
  ): Type[X] => Declaration[X] =
    (typeExp: Type[X]) => Declaration.TypeAliasDeclaration(typeParams, typeExp)

  def opaqueTypeDeclaration[X](typeParams: List[Name]): Declaration[X] =
    Declaration.OpaqueTypeDeclaration(typeParams)

  def customTypeDeclaration[X](typeParams: List[Name])(
      ctors: Constructors[X]
  ): Declaration[X] =
    CustomTypeDeclaration(typeParams, ctors)

  def customTypeDeclaration[X](
      typeParams: List[Name]
  ): Constructors[X] => Declaration[X] =
    (
      ctors: Constructors[X]
    ) => CustomTypeDeclaration(typeParams, ctors)

  case class Variable[A](name: Name, extra: A) extends Type[A] {

    override def jsonEncode(
        implicit extraEncoder: Writer[A]
    ): ujson.Value =
      ujson.Obj(
        typeTag("variable"),
        "name" -> name.jsonEncode,
        "extra" -> writeJs(extra)
      )

  }

  object Variable {
    implicit def readWriter[X: ReadWriter]: RW[Variable[X]] =
      readwriter[ujson.Value].bimap[Variable[X]](
        variable => variable.jsonEncode,
        jsonDecode[X]
      )

    def jsonDecode[X](json: JE.Value)(implicit extraDecoder: Reader[X]) = ???

  }
  case class Reference[A](
      typeName: FQName,
      typeParameters: List[Type[A]],
      extra: A
  ) extends Type[A]

  object Reference {}

  case class Tuple[A](elementTypes: List[Type[A]], extra: A) extends Type[A]
  case class Record[A](fieldTypes: List[Field[A]], extra: A) extends Type[A]
  case class ExtensibleRecord[A]() extends Type[A]
  case class Function[A](argType: Type[A], returnType: Type[A], extra: A)
      extends Type[A]
  case class Unit[A](extra: A) extends Type[A]

  case class Field[A](name: Name, fieldType: Type[A])

  sealed abstract class Declaration[X]
  object Declaration {
    case class TypeAliasDeclaration[X](
        typeParams: List[Name],
        typeExp: Type[X]
    ) extends Declaration[X]

    case class OpaqueTypeDeclaration[X](typeParams: List[Name])
        extends Declaration[X]
    case class CustomTypeDeclaration[X](
        typeParams: List[Name],
        ctors: Constructors[X]
    ) extends Declaration[X]

  }

  sealed abstract class Definition[+X]
  object Definition {
    case class TypeAliasDefinition[X](typeParams: List[Name], typeExp: Type[X])
        extends Definition[X]

    case class CustomTypeDefinition[X](typeParams: List[Name], ctors: Type[X])
        extends Definition[X]
  }

  case class Constructor[X](name: Name, args: List[(Name, Type[X])])
  case class Constructors[X](items: List[Constructor[X]]) extends AnyVal

  def typeTag(tag: String) =
    typeKey -> ujson.Str(tag)
}
