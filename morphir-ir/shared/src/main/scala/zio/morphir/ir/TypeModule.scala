package zio.morphir.ir

import zio.{Chunk, ZEnvironment}
import zio.prelude.*
import zio.morphir.syntax.TypeModuleSyntax

object TypeModule extends TypeModuleSyntax {

  final case class Constructors[+Annotations](items: Map[Name, TypeArg[Annotations]])

  sealed trait Definition[+Annotations] { self =>
    import Definition._
    import Specification._

    def toSpecification: Specification[Annotations] = self match {
      case TypeAlias(typeParams, typeExp) =>
        TypeAliasSpecification[Annotations](typeParams, typeExp, typeExp.annotations)
      case CustomType(typeParams: Chunk[Name], ctors) if ctors.withPublicAccess.isDefined =>
        val constructors: Constructors[Annotations] = ctors.withPublicAccess.get
        // val annotations = constructors.items.values.map(_.tpe.annotations).reduce(_ ++ _) // ???
        CustomTypeSpecification[Annotations](typeParams, constructors, ???)
      case CustomType(typeParams, _) =>
        OpaqueTypeSpecification[Annotations](typeParams, ???) // TODO fix annotations
    }

    // def eraseAttributes: Definition[Nothing] = self match {
    //   case TypeAlias(typeParams, typeExp) =>
    //     TypeAlias(typeParams, typeExp.eraseAttributes)
    //   case CustomType(typeParams, ctors) =>
    //     CustomType(typeParams, ctors.eraseAttributes)
    // }
  }

  object Definition {
    final case class TypeAlias[+Annotations](typeParams: Chunk[Name], typeExp: Type[Annotations])
        extends Definition[Annotations]

    final case class CustomType[+Annotations](
        typeParams: Chunk[Name],
        ctors: AccessControlled[Constructors[Annotations]]
    ) extends Definition[Annotations]
  }

  sealed trait Specification[+Annotations] { self =>
    // import Specification._

    def annotations: ZEnvironment[Annotations]

    // def map[Annotations0 >: Annotations](f: Annotations => Annotations0): Specification[Annotations0] = self match {
    //   case c @ TypeAliasSpecification(_, _, _) =>
    //     TypeAliasSpecification[Annotations0](c.typeParams, c.expr.map(f), c.annotations.map(f))
    //   case c @ OpaqueTypeSpecification(_, _) =>
    //     OpaqueTypeSpecification[Annotations0](c.typeParams, c.annotations.map(f))
    //   case c @ CustomTypeSpecification(_, _, _) =>
    //     CustomTypeSpecification[Annotations0](c.typeParams, c.ctors.map(f), c.annotations.map(f))
    // }
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
        ctors: Constructors[Annotations],
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

    // TODO
    // def mapTypeAttributes[B](f: Annotations => B): Type[B] =
    //   Type(self.caseValue.map(f), annotations.map(f))

    def collectVariables: Set[Name] = self.caseValue match {
      case TypeCase.ExtensibleRecordCase(_, fields) =>
        fields.flatMap(_.collectVariables).toSet
      case TypeCase.FieldCase(_, fieldType) =>
        fieldType.collectVariables
      case TypeCase.FunctionCase(paramTypes, returnType) =>
        paramTypes.flatMap(_.collectVariables).toSet ++ returnType.collectVariables
      case TypeCase.RecordCase(fields) =>
        fields.flatMap(_.collectVariables).toSet
      case TypeCase.ReferenceCase(_, typeParams) =>
        typeParams.flatMap(_.collectVariables).toSet
      case TypeCase.TupleCase(elementTypes) =>
        elementTypes.flatMap(_.collectVariables).toSet
      case TypeCase.UnitCase =>
        Set.empty
      case TypeCase.VariableCase(name) =>
        Set(name)
    }

    def collectReferences: Set[FQName] = self.caseValue match {
      case TypeCase.ExtensibleRecordCase(_, fields) =>
        fields.flatMap(_.collectReferences).toSet
      case TypeCase.FieldCase(_, fieldType) =>
        fieldType.collectReferences
      case TypeCase.FunctionCase(paramTypes, returnType) =>
        paramTypes.flatMap(_.collectReferences).toSet ++ returnType.collectReferences
      case TypeCase.RecordCase(fields) =>
        fields.flatMap(_.collectReferences).toSet
      case TypeCase.ReferenceCase(name, typeParams) =>
        typeParams.flatMap(_.collectReferences).toSet + name
      case TypeCase.TupleCase(elementTypes) =>
        elementTypes.flatMap(_.collectReferences).toSet
      case TypeCase.UnitCase =>
        Set.empty
      case TypeCase.VariableCase(_) =>
        Set.empty
    }

    // TO DO
    // def substituteTypeVariables(mapping: Map[Name, Type[Annotations]]): Type[Annotations] = self.caseValue match {
    //   case TypeCase.VariableCase(name) =>
    //     mapping.getOrElse(name, self)
    //   case TypeCase.ExtensibleRecordCase(name, fields) =>
    //     TypeCase.ExtensibleRecordCase(name, fields.map(_.substituteTypeVariables(mapping)))
    //   case TypeCase.FieldCase(name, fieldType) =>
    //     TypeCase.FieldCase(name, fieldType.substituteTypeVariables(mapping))
    //   case TypeCase.FunctionCase(paramTypes, returnType) =>
    //     TypeCase.FunctionCase(paramTypes.map(_.substituteTypeVariables(mapping)), returnType.substituteTypeVariables(mapping))
    //   case TypeCase.RecordCase(fields) =>
    //     TypeCase.RecordCase(fields.map(_.substituteTypeVariables(mapping)))
    //   case TypeCase.ReferenceCase(fqName, typeParams) =>
    //     TypeCase.ReferenceCase(fqName, typeParams.map(_.substituteTypeVariables(mapping)))
    //   case TypeCase.TupleCase(elementTypes) =>
    //     TypeCase.TupleCase(elementTypes.map(_.substituteTypeVariables(mapping)))
    //   case TypeCase.UnitCase =>
    //     TypeCase.UnitCase
    // }
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

    def field(name: Name, fieldType: Type[Any]): Field[Any] = Field(name, fieldType, ZEnvironment.empty)
    def record(fields: Chunk[Field[Any]]): Record[Any]      = Record(fields, ZEnvironment.empty)
    def tuple(elementTypes: Chunk[Type[Any]]): Tuple[Any]   = Tuple(elementTypes, ZEnvironment.empty)
    def function(paramTypes: Chunk[Type[Any]], returnType: Type[Any]): Function[Any] =
      Function(paramTypes, returnType, ZEnvironment.empty)
    def extensibleRecord(name: Name, fields: Chunk[Field[Any]]): ExtensibleRecord[Any] =
      ExtensibleRecord(name, fields, ZEnvironment.empty)
    def reference(name: FQName, typeParams: Chunk[Type[Any]]): Reference[Any] =
      Reference(name, typeParams, ZEnvironment.empty)

    final case class Unit[+Annotations](annotations: ZEnvironment[Annotations]) extends Type[Annotations] {
      override val caseValue: TypeCase[Type[Annotations]] = UnitCase

      override def toString: String = "()"
    }

    final case class ExtensibleRecord[+Annotations](
        name: Name,
        fields: Chunk[Field[Annotations]],
        annotations: ZEnvironment[Annotations]
    ) extends Type[Annotations] {
      override lazy val caseValue: ExtensibleRecordCase[Type[Annotations]] = ExtensibleRecordCase(name, fields)

      override def toString: String =
        s"{ ${name.toCamelCase} | ${fields.map(_.toString).mkString(", ")} }"
    }

    object ExtensibleRecord {
      object Case {
        def unapply[Annotations](
            extensibleRecord: ExtensibleRecord[Annotations]
        ): Option[ExtensibleRecordCase[Type[Annotations]]] =
          Some(extensibleRecord.caseValue)
      }
    }

    final case class Field[+Annotations](
        name: Name,
        fieldType: Type[Annotations],
        annotations: ZEnvironment[Annotations]
    ) extends Type[Annotations] { self =>
      override lazy val caseValue: FieldCase[Type[Annotations]] = FieldCase(name, fieldType)

      override def toString: String =
        s"${name.toCamelCase} : ${fieldType.toString}"

      def mapFieldType[Annotations0 >: Annotations](f: Type[Annotations0] => Type[Annotations0]): Field[Annotations0] =
        Field(name, f(fieldType), annotations)

      def mapFieldName[Annotations0 >: Annotations](f: Name => Name): Field[Annotations0] =
        Field(f(name), fieldType, annotations)
    }

    object Field {
      object Case {
        def unapply[Annotations](field: Field[Annotations]): Option[FieldCase[Type[Annotations]]] =
          Some(field.caseValue)
      }
    }

    final case class Function[+Annotations](
        paramTypes: Chunk[Type[Annotations]],
        returnType: Type[Annotations],
        annotations: ZEnvironment[Annotations]
    ) extends Type[Annotations] {
      override lazy val caseValue: FunctionCase[Type[Annotations]] = FunctionCase(paramTypes, returnType)

      override def toString: String =
        paramTypes
          .map(_.toString)
          .mkString("(", ",", ")")
          .concat(" -> " + returnType.toString)
    }

    object Function {
      object Case {
        def unapply[Annotations](function: Function[Annotations]): Option[FunctionCase[Type[Annotations]]] =
          Some(function.caseValue)
      }
    }

    final case class Record[+Annotations](
        fields: Chunk[Field[Annotations]],
        annotations: ZEnvironment[Annotations]
    ) extends Type[Annotations] {
      override lazy val caseValue: RecordCase[Type[Annotations]] = RecordCase(fields)

      override def toString: String =
        fields.map(_.toString).mkString("{ ", ", ", " }")
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

      override def toString: String =
        s"${name.toString} ${typeParams.map(_.toString).mkString(" ")}"
    }

    object Reference {
      object Case {
        def unapply[Annotations](reference: Reference[Annotations]): Option[ReferenceCase[Type[Annotations]]] =
          Some(reference.caseValue)
      }
    }

    final case class Tuple[+Annotations](
        typeParams: Chunk[Type[Annotations]],
        annotations: ZEnvironment[Annotations]
    ) extends Type[Annotations] {
      override lazy val caseValue: TupleCase[Type[Annotations]] = TupleCase(typeParams)

      override def toString: String =
        typeParams.map(_.toString).mkString("(", ", ", ")")
    }

    object Tuple {
      object Case {
        def unapply[Annotations](tuple: Tuple[Annotations]): Option[TupleCase[Type[Annotations]]] =
          Some(tuple.caseValue)
      }
    }

    final case class Variable[+Annotations](name: Name, annotations: ZEnvironment[Annotations])
        extends Type[Annotations] {
      override lazy val caseValue: VariableCase = VariableCase(name)

      override def toString: String = name.toCamelCase
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
    final case class FunctionCase[+Self](paramTypes: Chunk[Self], returnType: Self)  extends TypeCase[Self]
    final case class RecordCase[+Self](fields: Chunk[Self])                          extends TypeCase[Self]
    final case class ReferenceCase[+Self](typeName: FQName, typeParams: Chunk[Self]) extends TypeCase[Self]
    final case class TupleCase[+Self](elementTypes: Chunk[Self])                     extends TypeCase[Self]
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
