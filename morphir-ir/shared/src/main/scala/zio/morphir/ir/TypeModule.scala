package zio.morphir.ir

import zio.{Chunk, ZIO}
import zio.prelude._
import zio.morphir.syntax.TypeModuleSyntax
import zio.prelude.fx.ZPure
object TypeModule extends TypeModuleSyntax {

  final case class Field[+T](name: Name, fieldType: T) { self =>

    /**
     * An alias for `attributeTypeWith`.
     */
    def @@[Attributes0, Attributes](f: Attributes0 => Attributes)(implicit
        ev: T <:< Type[Attributes0]
    ): Field[Type[Attributes]] =
      attributeTypeWith(f)

    /**
     * Attributes the field with the given `attributes`.
     */
    def attributeTypeAs[Attributes](attributes: => Attributes)(implicit ev: T <:< Type[_]): Field[Type[Attributes]] =
      Field(name, fieldType.mapAttributes(_ => attributes))

    /**
     * Attributes the field's type using the given function.
     */
    def attributeTypeWith[Attributes0, Attributes](f: Attributes0 => Attributes)(implicit
        ev: T <:< Type[Attributes0]
    ): Field[Type[Attributes]] =
      Field(name, fieldType.mapAttributes(f))

    def forEach[G[+_]: IdentityBoth: Covariant, U](f: T => G[U]): G[Field[U]] =
      f(self.fieldType).map(newType => self.copy(fieldType = newType))

    def map[U](f: T => U): Field[U] = Field(name, f(fieldType))

  }

  final case class Constructors[+Attributes](items: Map[Name, Chunk[(Name, Type[Attributes])]]) { self =>
    def eraseAttributes: Constructors[Any] = Constructors(items.map { case (ctor, args) =>
      (ctor, args.map { case (paramName, paramType) => (paramName, paramType.eraseAttributes) })
    })

    def collectReferences: Set[FQName] = {
      items.values.flatMap {
        case Chunk((_, tpe)) =>
          tpe.collectReferences
        case _ => Nil
      }.toSet
    }
  }

  sealed trait Definition[+Attributes] { self =>
    import Definition._
    import Specification._

    def toSpecification: Specification[Attributes] = self match {
      case TypeAlias(typeParams, typeExp) =>
        TypeAliasSpecification[Attributes](typeParams, typeExp)
      case CustomType(typeParams: Chunk[Name], ctors) if ctors.withPublicAccess.isDefined =>
        val constructors: Constructors[Attributes] = ctors.withPublicAccess.get
        // val annotations = constructors.items.values.map(_.tpe.annotations).reduce(_ ++ _) // ???
        CustomTypeSpecification[Attributes](typeParams, constructors)
      case CustomType(typeParams, _) =>
        OpaqueTypeSpecification(typeParams) // TODO fix annotations
    }

    // def eraseAttributes: Definition[Nothing] = self match {
    //   case TypeAlias(typeParams, typeExp) =>
    //     TypeAlias(typeParams, typeExp.eraseAttributes)
    //   case CustomType(typeParams, ctors) =>
    //     CustomType(typeParams, ctors.eraseAttributes)
    // }
  }

  object Definition {
    final case class TypeAlias[+Attributes](typeParams: Chunk[Name], typeExp: Type[Attributes])
        extends Definition[Attributes]

    final case class CustomType[+Attributes](
        typeParams: Chunk[Name],
        ctors: AccessControlled[Constructors[Attributes]]
    ) extends Definition[Attributes]
  }

  type USpecification = Specification[Any]
  val USpecification = Specification
  sealed trait Specification[+Attributes] { self =>
    import Specification._

    def ??(doc: String): Documented[Specification[Attributes]] =
      Documented(doc, self)

    // def map[Annotations0 >: Annotations](f: Annotations => Annotations0): Specification[Annotations0] = self match {
    //   case c @ TypeAliasSpecification(_, _, _) =>
    //     TypeAliasSpecification[Annotations0](c.typeParams, c.expr.map(f), c.annotations.map(f))
    //   case c @ OpaqueTypeSpecification(_, _) =>
    //     OpaqueTypeSpecification[Annotations0](c.typeParams, c.annotations.map(f))
    //   case c @ CustomTypeSpecification(_, _, _) =>
    //     CustomTypeSpecification[Annotations0](c.typeParams, c.ctors.map(f), c.annotations.map(f))
    // }
    def eraseAttributes: Specification[Any] = self match {
      case c @ TypeAliasSpecification(_, _) =>
        TypeAliasSpecification(c.typeParams, c.expr.eraseAttributes)
      case c @ OpaqueTypeSpecification(_) =>
        OpaqueTypeSpecification(c.typeParams)
      case c @ CustomTypeSpecification(_, _) =>
        CustomTypeSpecification(c.typeParams, c.ctors.eraseAttributes)
    }
  }

  object Specification {
    final case class TypeAliasSpecification[+Attributes](
        typeParams: Chunk[Name],
        expr: Type[Attributes]
    ) extends Specification[Attributes]

    final case class OpaqueTypeSpecification(typeParams: Chunk[Name]) extends Specification[Nothing]

    object OpaqueTypeSpecification {
      def apply(typeParams: String*): OpaqueTypeSpecification =
        OpaqueTypeSpecification(Chunk.fromIterable(typeParams.map(Name.fromString)))
    }

    final case class CustomTypeSpecification[+Attributes](
        typeParams: Chunk[Name],
        ctors: Constructors[Attributes]
    ) extends Specification[Attributes]
  }

  final case class Type[+Attributes] private[morphir] (
      caseValue: TypeCase[Type[Attributes]],
      attributes: Attributes
  ) {
    self =>

    def @@[Attributes2](f: Attributes => Attributes2): Type[Attributes2] =
      mapAttributes(f)

    def ??(doc: String): Documented[Type[Attributes]] = Documented(doc, self)

    def fold[Z](f: TypeCase[Z] => Z): Z =
      foldAttributed((typeCase, _) => f(typeCase))

    def foldAttributed[Z](f: (TypeCase[Z], Attributes) => Z): Z =
      self.caseValue match {
        case c @ TypeCase.ExtensibleRecordCase(_, _) =>
          f(TypeCase.ExtensibleRecordCase(c.name, c.fields.map(field => field.map(_.foldAttributed(f)))), attributes)
        case c @ TypeCase.FunctionCase(_, _) =>
          f(TypeCase.FunctionCase(c.paramTypes.map(_.foldAttributed(f)), c.returnType.foldAttributed(f)), attributes)
        case c @ TypeCase.RecordCase(_) =>
          f(TypeCase.RecordCase(c.fields.map(field => field.map(_.foldAttributed(f)))), attributes)
        case c @ TypeCase.ReferenceCase(_, _) =>
          f(TypeCase.ReferenceCase(c.typeName, c.typeParams.map(_.foldAttributed(f))), attributes)
        case c @ TypeCase.TupleCase(_)    => f(TypeCase.TupleCase(c.elementTypes.map(_.foldAttributed(f))), attributes)
        case _ @TypeCase.UnitCase         => f(TypeCase.UnitCase, attributes)
        case c @ TypeCase.VariableCase(_) => f(c, attributes)
      }

    def foldDown[Z](z: Z)(f: (Z, Type[Attributes]) => Z): Z =
      caseValue.foldLeft(f(z, self))((z, recursive) => recursive.foldDown(z)(f))

    def foldDownSome[Z](z: Z)(pf: PartialFunction[(Z, Type[Attributes]), Z]): Z =
      foldDown(z)((z, recursive) => pf.lift(z -> recursive).getOrElse(z))

    def foldM[F[+_]: AssociativeFlatten: Covariant: IdentityBoth, Z](f: TypeCase[Z] => F[Z]): F[Z] =
      fold[F[Z]](_.flip.flatMap(f))

    def foldPure[W, S, R, E, Z](f: TypeCase[Z] => ZPure[W, S, S, R, E, Z]): ZPure[W, S, S, R, E, Z] =
      foldM(f)

    def transformDown[Annotations0 >: Attributes](
        f: Type[Annotations0] => Type[Annotations0]
    ): Type[Annotations0] = {
      def loop(recursive: Type[Annotations0]): Type[Attributes] =
        Type(f(recursive).caseValue.map(loop), attributes)
      loop(self)
    }

    def foldZIO[R, E, Z](f: TypeCase[Z] => ZIO[R, E, Z]): ZIO[R, E, Z] =
      foldM(f)

    def foldRecursive[Z](f: TypeCase[(Type[Attributes], Z)] => Z): Z =
      f(caseValue.map(recursive => recursive -> recursive.foldRecursive(f)))

    def foldUp[Z](z: Z)(f: (Z, Type[Attributes]) => Z): Z =
      f(caseValue.foldLeft(z)((z, recursive) => recursive.foldUp(z)(f)), self)

    def foldUpSome[Z](z: Z)(pf: PartialFunction[(Z, Type[Attributes]), Z]): Z =
      foldUp(z)((z, recursive) => pf.lift(z -> recursive).getOrElse(z))

    def mapAttributes[Attributes2](f: Attributes => Attributes2): Type[Attributes2] =
      self.foldAttributed[Type[Attributes2]] { case (typeCase, attributes) =>
        Type(typeCase, f(attributes))
      }

    def collectVariables: Set[Name] = fold[Set[Name]] {
      case c @ TypeCase.ExtensibleRecordCase(_, _)       => c.fields.map(_.fieldType).flatten.toSet + c.name
      case TypeCase.FunctionCase(paramTypes, returnType) => paramTypes.flatten.toSet ++ returnType
      case TypeCase.RecordCase(fields)                   => fields.map(_.fieldType).flatten.toSet
      case TypeCase.ReferenceCase(_, typeParams)         => typeParams.flatten.toSet
      case TypeCase.TupleCase(elementTypes)              => elementTypes.flatten.toSet
      case TypeCase.UnitCase                             => Set.empty
      case TypeCase.VariableCase(name)                   => Set(name)
    }

    def collectReferences: Set[FQName] = fold[Set[FQName]] {
      case c @ TypeCase.ExtensibleRecordCase(_, _) => c.fields.map(_.fieldType).flatten.toSet
      case TypeCase.FunctionCase(paramTypes, returnType) =>
        paramTypes.flatten.toSet ++ returnType
      case TypeCase.RecordCase(fields) =>
        fields.map(_.fieldType).flatten.toSet
      case TypeCase.ReferenceCase(name, typeParams) =>
        typeParams.flatten.toSet + name
      case TypeCase.TupleCase(elementTypes) => elementTypes.flatten.toSet
      case TypeCase.UnitCase                => Set.empty
      case TypeCase.VariableCase(_)         => Set.empty
    }

    /**
     * Erase the attributes from this type.
     */
    def eraseAttributes: UType = self.mapAttributes(_ => Type.emptyAttributes)

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

    def satisfiesCaseOf(check: PartialFunction[TypeCase[Type[Attributes]], Boolean]): Boolean =
      check.lift(self.caseValue).getOrElse(false)

    override def toString: String = fold[String] {
      case c @ TypeCase.ExtensibleRecordCase(_, _) =>
        s"{ ${c.name.toCamelCase} | ${c.fields.mkString(", ")} }"
      case TypeCase.FunctionCase(paramTypes, returnType) =>
        paramTypes
          .map(_.toString)
          .mkString("(", ",", ")")
          .concat(" -> " + returnType.toString)
      case TypeCase.RecordCase(fields)              => fields.mkString("{ ", ", ", " }")
      case TypeCase.ReferenceCase(name, typeParams) => s"${name.toString} ${typeParams.mkString(" ")}"
      case TypeCase.TupleCase(elementTypes)         => elementTypes.mkString("(", ", ", ")")
      case TypeCase.UnitCase                        => "()"
      case TypeCase.VariableCase(name)              => name.toCamelCase
    }
  }

  object Type extends TypeModuleSyntax {
    import TypeCase._

    lazy val emptyAttributes: Any = ()

    def apply(caseValue: TypeCase[UType]): UType = Type(caseValue, emptyAttributes)

    object Variable {
      def unapply(tpe: Type[Any]): Option[Name] = tpe.caseValue match {
        case VariableCase(name) => Some(name)
        case _                  => None
      }
    }
  }

  sealed trait TypeCase[+Self] { self =>
    import TypeCase._

    def map[B](f: Self => B): TypeCase[B] = self match {
      case c @ ExtensibleRecordCase(_, _) => ExtensibleRecordCase(c.name, c.fields.map(_.map(f)))
      case c @ FunctionCase(_, _)         => FunctionCase(c.paramTypes.map(f), f(c.returnType))
      case c @ ReferenceCase(_, _)        => ReferenceCase(c.typeName, c.typeParams.map(f))
      case c @ TupleCase(_)               => TupleCase(c.elementTypes.map(f))
      case UnitCase                       => UnitCase
      case c @ VariableCase(_)            => VariableCase(c.name)
      case c @ RecordCase(_)              => RecordCase(c.fields.map(_.map(f)))
    }
  }

  object TypeCase {
    final case class ExtensibleRecordCase[+Self](name: Name, fields: Chunk[Field[Self]]) extends TypeCase[Self]
    final case class FunctionCase[+Self](paramTypes: Chunk[Self], returnType: Self)      extends TypeCase[Self]
    final case class RecordCase[+Self](fields: Chunk[Field[Self]])                       extends TypeCase[Self]
    final case class ReferenceCase[+Self](typeName: FQName, typeParams: Chunk[Self])     extends TypeCase[Self]
    final case class TupleCase[+Self](elementTypes: Chunk[Self])                         extends TypeCase[Self]
    case object UnitCase                                                                 extends TypeCase[Nothing]
    final case class VariableCase(name: Name)                                            extends TypeCase[Nothing]

    implicit val TypeCaseForEach: ForEach[TypeCase] =
      new ForEach[TypeCase] {
        def forEach[G[+_]: IdentityBoth: Covariant, A, B](fa: TypeCase[A])(f: A => G[B]): G[TypeCase[B]] =
          fa match {
            case ExtensibleRecordCase(name, fields) =>
              fields.forEach(_.forEach(f)).map(fields => ExtensibleRecordCase(name, fields))
            case FunctionCase(paramTypes, returnType) =>
              paramTypes
                .forEach(f)
                .zipWith(f(returnType))((paramTypes, returnType) => FunctionCase(paramTypes, returnType))
            case RecordCase(fields) =>
              fields.forEach(_.forEach(f)).map(fields => RecordCase(fields))
            case ReferenceCase(typeName, typeParams) =>
              typeParams.forEach(f).map(typeParams => ReferenceCase(typeName, typeParams))
            case TupleCase(elementTypes) =>
              elementTypes.forEach(f).map(elementTypes => TupleCase(elementTypes))
            case UnitCase =>
              UnitCase.succeed
            case VariableCase(name) =>
              VariableCase(name).succeed
          }
      }
  }

  /** Represents an un-annotated type. */
  type UType = Type[Any]
  val UType = Type
}
