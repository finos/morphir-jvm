package zio.morphir.ir

import zio.{Chunk, ZEnvironment, ZIO}
import zio.prelude.*
import zio.morphir.syntax.TypeModuleSyntax
import zio.prelude.fx.ZPure

object TypeModule extends TypeModuleSyntax {

  final case class Field[+T](name: Name, tpe: T) { self =>
    def forEach[G[+_]: IdentityBoth: Covariant, U](f: T => G[U]): G[Field[U]] =
      f(self.tpe).map(newType => self.copy(tpe = newType))

    def map[U](f: T => U): Field[U] = Field(name, f(tpe))

  }

  final case class Constructors[+Annotations](items: Map[Name, Chunk[(Name, Type[Annotations])]]) { self =>
    def toUnannotated: Constructors[Any] = Constructors(items.map { case (ctor, args) =>
      (ctor, args.map { case (paramName, paramType) => (paramName, paramType.toUnannotated) })
    })

    def collectReferences: Set[FQName] = {
      items.values.flatMap {
        case Chunk((_, tpe)) =>
          tpe.collectReferences
        case _ => Nil
      }.toSet
    }
  }

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

  type USpecification = Specification[Any]
  val USpecification = Specification
  sealed trait Specification[+Annotations] { self =>
    import Specification.*

    def annotations: ZEnvironment[Annotations]

    // def map[Annotations0 >: Annotations](f: Annotations => Annotations0): Specification[Annotations0] = self match {
    //   case c @ TypeAliasSpecification(_, _, _) =>
    //     TypeAliasSpecification[Annotations0](c.typeParams, c.expr.map(f), c.annotations.map(f))
    //   case c @ OpaqueTypeSpecification(_, _) =>
    //     OpaqueTypeSpecification[Annotations0](c.typeParams, c.annotations.map(f))
    //   case c @ CustomTypeSpecification(_, _, _) =>
    //     CustomTypeSpecification[Annotations0](c.typeParams, c.ctors.map(f), c.annotations.map(f))
    // }
    def toUnannotated: Specification[Any] = self match {
      case c @ TypeAliasSpecification(_, _, _) =>
        TypeAliasSpecification(c.typeParams, c.expr.toUnannotated, ZEnvironment.empty)
      case c @ OpaqueTypeSpecification(_, _) =>
        OpaqueTypeSpecification[Any](c.typeParams, ZEnvironment.empty)
      case c @ CustomTypeSpecification(_, _, _) =>
        CustomTypeSpecification[Any](c.typeParams, c.ctors.toUnannotated, ZEnvironment.empty)
    }
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

  final case class Type[+Annotations] private[morphir] (
      caseValue: TypeCase[Type[Annotations]],
      annotations: ZEnvironment[Annotations]
  ) {
    self =>
    // import TypeCase.*

    def ??(doc: String): Documented[Type[Annotations]] = Documented(doc, self)

    final def asType: Type[Annotations] = self

    def fold[Z](f: TypeCase[Z] => Z): Z =
      foldAnnotated((typeCase, _) => f(typeCase))

    def foldAnnotated[Z](f: (TypeCase[Z], ZEnvironment[Annotations]) => Z): Z =
      self.caseValue match {
        case c @ TypeCase.ExtensibleRecordCase(_, _) =>
          f(TypeCase.ExtensibleRecordCase(c.name, c.fields.map(field => field.map(_.foldAnnotated(f)))), annotations)
        case c @ TypeCase.FunctionCase(_, _) =>
          f(TypeCase.FunctionCase(c.paramTypes.map(_.foldAnnotated(f)), c.returnType.foldAnnotated(f)), annotations)
        case c @ TypeCase.RecordCase(_) =>
          f(TypeCase.RecordCase(c.fields.map(field => field.map(_.foldAnnotated(f)))), annotations)
        case c @ TypeCase.ReferenceCase(_, _) =>
          f(TypeCase.ReferenceCase(c.typeName, c.typeParams.map(_.foldAnnotated(f))), annotations)
        case c @ TypeCase.TupleCase(_)    => f(TypeCase.TupleCase(c.elementTypes.map(_.foldAnnotated(f))), annotations)
        case _ @TypeCase.UnitCase         => f(TypeCase.UnitCase, annotations)
        case c @ TypeCase.VariableCase(_) => f(c, annotations)
      }

    def foldDown[Z](z: Z)(f: (Z, Type[Annotations]) => Z): Z =
      caseValue.foldLeft(f(z, self))((z, recursive) => recursive.foldDown(z)(f))

    def foldDownSome[Z](z: Z)(pf: PartialFunction[(Z, Type[Annotations]), Z]): Z =
      foldDown(z)((z, recursive) => pf.lift(z -> recursive).getOrElse(z))

    def foldM[F[+_]: AssociativeFlatten: Covariant: IdentityBoth, Z](f: TypeCase[Z] => F[Z]): F[Z] =
      fold[F[Z]](_.flip.flatMap(f))

    def foldPure[W, S, R, E, Z](f: TypeCase[Z] => ZPure[W, S, S, R, E, Z]): ZPure[W, S, S, R, E, Z] =
      foldM(f)

    def transformDown[Annotations0 >: Annotations](
        f: Type[Annotations0] => Type[Annotations0]
    ): Type[Annotations0] = {
      def loop(recursive: Type[Annotations0]): Type[Annotations] =
        Type(f(recursive).caseValue.map(loop), annotations)
      loop(self)
    }

    def foldZIO[R, E, Z](f: TypeCase[Z] => ZIO[R, E, Z]): ZIO[R, E, Z] =
      foldM(f)

    def foldRecursive[Z](f: TypeCase[(Type[Annotations], Z)] => Z): Z =
      f(caseValue.map(recursive => recursive -> recursive.foldRecursive(f)))

    def foldUp[Z](z: Z)(f: (Z, Type[Annotations]) => Z): Z =
      f(caseValue.foldLeft(z)((z, recursive) => recursive.foldUp(z)(f)), self)

    def foldUpSome[Z](z: Z)(pf: PartialFunction[(Z, Type[Annotations]), Z]): Z =
      foldUp(z)((z, recursive) => pf.lift(z -> recursive).getOrElse(z))

    // TODO
    // def mapTypeAttributes[B](f: Annotations => B): Type[B] =
    //   Type(self.caseValue.map(f), annotations.map(f))

    def collectVariables: Set[Name] = fold[Set[Name]] {
      case c @ TypeCase.ExtensibleRecordCase(_, _)       => c.fields.map(_.tpe).flatten.toSet + c.name
      case TypeCase.FunctionCase(paramTypes, returnType) => paramTypes.flatten.toSet ++ returnType
      case TypeCase.RecordCase(fields)                   => fields.map(_.tpe).flatten.toSet
      case TypeCase.ReferenceCase(_, typeParams)         => typeParams.flatten.toSet
      case TypeCase.TupleCase(elementTypes)              => elementTypes.flatten.toSet
      case TypeCase.UnitCase                             => Set.empty
      case TypeCase.VariableCase(name)                   => Set(name)
    }

    def collectReferences: Set[FQName] = fold[Set[FQName]] {
      case c @ TypeCase.ExtensibleRecordCase(_, _) => c.fields.map(_.tpe).flatten.toSet
      case TypeCase.FunctionCase(paramTypes, returnType) =>
        paramTypes.flatten.toSet ++ returnType
      case TypeCase.RecordCase(fields) =>
        fields.map(_.tpe).flatten.toSet
      case TypeCase.ReferenceCase(name, typeParams) =>
        typeParams.flatten.toSet + name
      case TypeCase.TupleCase(elementTypes) => elementTypes.flatten.toSet
      case TypeCase.UnitCase                => Set.empty
      case TypeCase.VariableCase(_)         => Set.empty
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

    def satisfiesCaseOf(check: PartialFunction[TypeCase[Type[Annotations]], Boolean]): Boolean =
      check.lift(self.caseValue).getOrElse(false)

    def toUnannotated: UType =
      Type(self.caseValue.map(t => t.copy(annotations = ZEnvironment.empty)))

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
    import TypeCase.*

    private[morphir] def apply(
        caseValue: TypeCase[Type[Any]]
    ): Type[Any] = Type(caseValue, ZEnvironment.empty)

    object Variable {
      def unapply(tpe: Type[Any]): Option[Name] = tpe.caseValue match {
        case VariableCase(name) => Some(name)
        case _                  => None
      }
    }

  }

  sealed trait TypeCase[+Self] { self =>
    import TypeCase.*

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
