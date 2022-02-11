package zio.morphir.ir

import zio.{Chunk, ZEnvironment}
import zio.prelude.*
import zio.morphir.syntax.TypeModuleSyntax

object TypeModule extends TypeModuleSyntax {

  final case class Constructors[+Annotations](items: Map[Name, TypeArg[Annotations]])

  sealed trait Definition[+Annotations] { self =>
    def toSpecification: Specification[Annotations] = ???
  }
  object Definition {
    final case class TypeAlias[+Annotations](typeParams: Chunk[Name], typeExp: Type[Annotations])
        extends Definition[Annotations]

    final case class CustomType[+Annotations](
        typeParams: Chunk[Name],
        ctors: AccessControlled[Constructors[Annotations]]
    ) extends Definition[Annotations]
  }
  sealed trait Specification[+Annotations] {
    def annotations: ZEnvironment[Annotations]
  }

  object Specification {
    final case class TypeAliasSpecification[+Annotations](
        typeParams: Chunk[Name],
        expr: Type[Annotations],
        annotations: ZEnvironment[Annotations]
    ) extends Specification[Annotations]

    final case class OpaqueTypeSpecification[+Annotations](
        typeParams: Chunk[Name],
        annotations: ZEnvironment[Annotations]
    ) extends Specification[Annotations]

    final case class CustomTypeSpecification[+Annotations](
        typeParams: Chunk[Name],
        expr: Type[Annotations],
        annotations: ZEnvironment[Annotations]
    ) extends Specification[Annotations]
  }
  sealed trait Type[+Annotations] { self =>
    // import TypeCase.*

    final def asType: Type[Annotations] = self

    def annotations: ZEnvironment[Annotations]
    def caseValue: TypeCase[Type[Annotations]]

    def fold[Z](f: TypeCase[Z] => Z): Z = self.caseValue match {
      case c @ TypeCase.ExtensibleRecordCase(_, _) => f(TypeCase.ExtensibleRecordCase(c.name, c.fields.map(_.fold(f))))
      case c @ TypeCase.FieldCase(_, _)            => f(TypeCase.FieldCase(c.name, c.fieldType.fold(f)))
      case c @ TypeCase.FunctionCase(_, _) =>
        f(TypeCase.FunctionCase(c.paramTypes.map(_.fold(f)), c.returnType.fold(f)))
      case c @ TypeCase.RecordCase(_)       => f(TypeCase.RecordCase(c.fields.map(_.fold(f))))
      case c @ TypeCase.ReferenceCase(_, _) => f(TypeCase.ReferenceCase(c.typeName, c.typeParams.map(_.fold(f))))
      case c @ TypeCase.TupleCase(_)        => f(TypeCase.TupleCase(c.elementTypes.map(_.fold(f))))
      case _ @TypeCase.UnitCase             => f(TypeCase.UnitCase)
      case c @ TypeCase.VariableCase(_)     => f(c)
    }

    def transformDown[Annotations0 >: Annotations](
        f: Type[Annotations0] => Type[Annotations0]
    ): Type[Annotations0] = {
      def loop(recursive: Type[Annotations0]): Type[Annotations] =
        Type(f(recursive).caseValue.map(loop), annotations)
      loop(self)
    }
  }

  object Type {
    import TypeCase.*

    def apply[Annotations](
        caseValue0: TypeCase[Type[Annotations]],
        annotations0: ZEnvironment[Annotations]
    ): Type[Annotations] =
      new Type[Annotations] {
        override def caseValue: TypeCase[Type[Annotations]] = caseValue0
        override def annotations: ZEnvironment[Annotations] = annotations0
      }

    def ref(name: FQName): Reference[Any] = Reference(name, Chunk.empty, ZEnvironment.empty)

    /**
     * Creates a type variable with the given `name`.
     */
    def variable(name: Name): Variable[Any]   = Variable(name, ZEnvironment.empty)
    def variable(name: String): Variable[Any] = variable(Name(name))
    val unit: Type[Any]                       = Unit(ZEnvironment.empty)

    final case class Unit[+Annotations](annotations: ZEnvironment[Annotations]) extends Type[Annotations] {
      override val caseValue: TypeCase[Type[Annotations]] = UnitCase
    }

    final case class Field[+Annotations](
        name: Name,
        fieldType: Type[Annotations],
        annotations: ZEnvironment[Annotations]
    ) extends Type[Annotations] {
      override lazy val caseValue: FieldCase[Type[Annotations]] = FieldCase(name, fieldType)
    }
    object Field {

      object Case {
        def unapply[Annotations](field: Field[Annotations]): Option[FieldCase[Type[Annotations]]] =
          Some(field.caseValue)
      }
    }

    final case class Record[+Annotations](
        fields: Chunk[Field[Annotations]],
        annotations: ZEnvironment[Annotations]
    ) extends Type[Annotations] {
      override lazy val caseValue: RecordCase[Type[Annotations]] = RecordCase(fields)
    }
    object Record {

      object Case {
        def unapply[Annotations](record: Record[Annotations]): Option[RecordCase[Type[Annotations]]] =
          Some(record.caseValue)
      }
    }

    final case class Reference[+Annotations](
        name: FQName,
        typeParams: Chunk[Type[Annotations]],
        annotations: ZEnvironment[Annotations]
    ) extends Type[Annotations] {
      override lazy val caseValue: ReferenceCase[Type[Annotations]] = ReferenceCase(name, typeParams)
    }

    object Reference {
      object Case {
        def unapply[Annotations](reference: Reference[Annotations]): Option[ReferenceCase[Type[Annotations]]] =
          Some(reference.caseValue)
      }
    }

    final case class Variable[+Annotations](name: Name, annotations: ZEnvironment[Annotations])
        extends Type[Annotations] {
      override lazy val caseValue: VariableCase = VariableCase(name)
    }
    object Variable {
      object Case {
        def unapply[Annotations](variable: Variable[Annotations]): Option[VariableCase] =
          Some(variable.caseValue)
      }
    }
  }

  sealed trait TypeCase[+Self] { self =>
    import TypeCase.*
    def map[B](f: Self => B): TypeCase[B] = self match {
      case c @ ExtensibleRecordCase(_, _) => ExtensibleRecordCase(c.name, c.fields.map(f))
      case c @ FieldCase(_, _)            => FieldCase(c.name, f(c.fieldType))
      case c @ FunctionCase(_, _)         => FunctionCase(c.paramTypes.map(f), f(c.returnType))
      case c @ ReferenceCase(_, _)        => ReferenceCase(c.typeName, c.typeParams.map(f))
      case c @ TupleCase(_)               => TupleCase(c.elementTypes.map(f))
      case UnitCase                       => UnitCase
      case c @ VariableCase(_)            => VariableCase(c.name)
      case c @ RecordCase(_)              => RecordCase(c.fields.map(f))
    }
  }

  object TypeCase {
    final case class ExtensibleRecordCase[+Self](name: Name, fields: Chunk[Self])    extends TypeCase[Self]
    final case class FunctionCase[+Self](paramTypes: List[Self], returnType: Self)   extends TypeCase[Self]
    final case class RecordCase[+Self](fields: Chunk[Self])                          extends TypeCase[Self]
    final case class ReferenceCase[+Self](typeName: FQName, typeParams: Chunk[Self]) extends TypeCase[Self]
    final case class TupleCase[+Self](elementTypes: List[Self])                      extends TypeCase[Self]
    case object UnitCase                                                             extends TypeCase[Nothing]
    final case class VariableCase(name: Name)                                        extends TypeCase[Nothing]
    final case class FieldCase[+Self](name: Name, fieldType: Self)                   extends TypeCase[Self]

    implicit val TypeCaseForEach: ForEach[TypeCase] =
      new ForEach[TypeCase] {
        def forEach[G[+_]: IdentityBoth: Covariant, A, B](fa: TypeCase[A])(f: A => G[B]): G[TypeCase[B]] =
          fa match {
            case ExtensibleRecordCase(name, fields) =>
              fields.forEach(f).map(fields => ExtensibleRecordCase(name, fields))
            case FunctionCase(paramTypes, returnType) =>
              paramTypes
                .forEach(f)
                .zipWith(f(returnType))((paramTypes, returnType) => FunctionCase(paramTypes, returnType))
            case RecordCase(fields) =>
              fields.forEach(f).map(fields => RecordCase(fields))
            case ReferenceCase(typeName, typeParams) =>
              typeParams.forEach(f).map(typeParams => ReferenceCase(typeName, typeParams))
            case TupleCase(elementTypes) =>
              elementTypes.forEach(f).map(elementTypes => TupleCase(elementTypes))
            case UnitCase =>
              UnitCase.succeed
            case VariableCase(name) =>
              VariableCase(name).succeed
            case FieldCase(name, fieldType) =>
              f(fieldType).map(fieldType => FieldCase(name, fieldType))
          }
      }
  }

  final case class TypeArg[+Annotations](
      name: Name,
      tpe: Type[Annotations]
  )

  /** Represents an un-annotated type. */
  type UType = Type[Any]
  val UType = Type
}
