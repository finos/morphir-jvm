package zio.morphir.ir.types.recursive

import zio.morphir.ir._
import zio.prelude._
import zio.prelude.fx.ZPure
import zio.{Chunk, ZIO}

final case class Type[+A](caseValue: TypeCase[A, Type[A]]) { self =>
  import TypeCase._
  import Type._

  def ??(doc: String): Documented[Type[A]] = Documented(doc, self)

  def attributes: A = caseValue.attributes

  def collectReferences: Set[FQName] = fold[Set[FQName]] {
    case TypeCase.ExtensibleRecordCase(_, _, fields)        => fields.map(_.data).flatten.toSet
    case TypeCase.FunctionCase(_, argumentType, returnType) => argumentType ++ returnType
    case TypeCase.RecordCase(_, fields)                     => fields.map(_.data).flatten.toSet
    case TypeCase.ReferenceCase(_, name, typeParams)        => typeParams.flatten.toSet + name
    case TypeCase.TupleCase(_, elementTypes)                => elementTypes.flatten.toSet
    case TypeCase.UnitCase(_)                               => Set.empty
    case TypeCase.VariableCase(_, _)                        => Set.empty
  }

  def collectVariables: Set[Name] = fold[Set[Name]] {
    case TypeCase.ExtensibleRecordCase(_, name, fields)     => fields.map(_.data).flatten.toSet + name
    case TypeCase.FunctionCase(_, argumentType, returnType) => argumentType ++ returnType
    case TypeCase.RecordCase(_, fields)                     => fields.map(_.data).flatten.toSet
    case TypeCase.ReferenceCase(_, _, typeParams)           => typeParams.flatten.toSet
    case TypeCase.TupleCase(_, elementTypes)                => elementTypes.flatten.toSet
    case TypeCase.UnitCase(_)                               => Set.empty
    case TypeCase.VariableCase(_, name)                     => Set(name)
  }

  /**
   * Erase the attributes from this type.
   */
  def eraseAttributes: UType = self.mapAttributes(_ => ())

  def fields: Chunk[Field[Type[A]]] = fold[Chunk[Field[Type[A]]]] {
    case ExtensibleRecordCase(_, _, fields) => fields.map(_.data).flatten
    case RecordCase(_, fields)              => fields.map(_.data).flatten
    case _                                  => Chunk.empty
  }

  def fieldCount: Int = fold[Int] {
    case ExtensibleRecordCase(_, _, fields) => fields.map(_.data).sum + fields.size
    case RecordCase(_, fields)              => fields.map(_.data).sum + fields.size
    case _                                  => 0
  }

  def fold[Z](f: TypeCase[A, Z] => Z): Z = caseValue match {
    case c @ ExtensibleRecordCase(_, _, _) =>
      f(ExtensibleRecordCase(c.attributes, c.name, c.fields.map(_.map(_.fold(f)))))
    case c @ FunctionCase(_, _, _)  => f(FunctionCase(c.attributes, c.argumentType.fold(f), c.returnType.fold(f)))
    case c @ RecordCase(_, _)       => f(RecordCase(c.attributes, c.fields.map(_.map(_.fold(f)))))
    case c @ ReferenceCase(_, _, _) => f(ReferenceCase(c.attributes, c.typeName, c.typeParams.map(_.fold(f))))
    case c @ TupleCase(_, _)        => f(TupleCase(c.attributes, c.elements.map(_.fold(f))))
    case c @ UnitCase(_)            => f(c)
    case c @ VariableCase(_, _)     => f(c)
  }

  def foldDown[Z](z: Z)(f: (Z, Type[A]) => Z): Z =
    caseValue.foldLeft(f(z, self))((z, recursive) => recursive.foldDown(z)(f))

  def foldDownSome[Z](z: Z)(pf: PartialFunction[(Z, Type[A]), Z]): Z =
    foldDown(z)((z, recursive) => pf.lift(z -> recursive).getOrElse(z))

  def foldM[F[+_]: AssociativeFlatten: Covariant: IdentityBoth, Z](f: TypeCase[A, Z] => F[Z]): F[Z] =
    fold[F[Z]](_.flip.flatMap(f))

  def foldPure[W, S, R, E, Z](f: TypeCase[A, Z] => ZPure[W, S, S, R, E, Z]): ZPure[W, S, S, R, E, Z] =
    foldM(f)

  def foldRecursive[Z](f: TypeCase[A, (Type[A], Z)] => Z): Z =
    f(caseValue.map(recursive => recursive -> recursive.foldRecursive(f)))

  def foldUp[Z](z: Z)(f: (Z, Type[A]) => Z): Z =
    f(caseValue.foldLeft(z)((z, recursive) => recursive.foldUp(z)(f)), self)

  def foldZIO[R, E, Z](f: TypeCase[A, Z] => ZIO[R, E, Z]): ZIO[R, E, Z] = foldM(f)

  def map[B](f: A => B): Type[B] =
    self.fold[Type[B]] {
      case ExtensibleRecordCase(attributes, name, fields) => Type(ExtensibleRecordCase(f(attributes), name, fields))
      case FunctionCase(attributes, paramTypes, returnType) =>
        Type(FunctionCase(f(attributes), paramTypes, returnType))
      case RecordCase(attributes, fields) => Type(RecordCase(f(attributes), fields))
      case ReferenceCase(attributes, typeName, typeParams) =>
        Type(ReferenceCase(f(attributes), typeName, typeParams))
      case TupleCase(attributes, elements) => Type(TupleCase(f(attributes), elements))
      case UnitCase(attributes)            => Type(UnitCase(f(attributes)))
      case VariableCase(attributes, name)  => Type(VariableCase(f(attributes), name))
    }

  @inline def mapAttributes[B](f: A => B): Type[B] = map(f)

  def satisfiesCaseOf(check: PartialFunction[TypeCase[A, Type[A]], Boolean]): Boolean =
    check.lift(self.caseValue).getOrElse(false)

  override def toString: String = foldRecursive[String] {
    case ExtensibleRecordCase(_, name, fields) =>
      val fieldList = fields.map(field => field.name.toCamelCase + " : " + field.data._2).mkString(", ")
      s"{ ${name.toCamelCase} | $fieldList }"
    case FunctionCase(_, (Function(_, _, _), argType), (_, returnType)) => s"($argType) -> $returnType"
    case FunctionCase(_, (_, argType), (_, returnType))                 => s"$argType -> $returnType"
    case RecordCase(_, fields) =>
      fields.map(field => field.name.toCamelCase + " : " + field.data._2).mkString("{ ", ", ", " }")
    case ReferenceCase(_, name, typeParams) => (name.toReferenceName +: typeParams.map(_._2.toString)).mkString(" ")
    case TupleCase(_, elementTypes)         => elementTypes.map(_._2).mkString("(", ", ", ")")
    case UnitCase(_)                        => "()"
    case VariableCase(_, name)              => name.toCamelCase
  }
}

object Type extends TypeExprConstructors with UnattributedTypeExprConstructors with FieldSyntax {
  import TypeCase._
  type FieldT[A] = Field[Type[A]]

  type UType = Type[Any]
  val UType = Type

  def mapTypeAttributes[A](tpe: Type[A]): MapTypeAttributes[A] = new MapTypeAttributes(() => tpe)

  object ExtensibleRecord {
    def apply[A](attributes: A, name: Name, fields: Chunk[FieldT[A]]): Type[A] =
      Type(ExtensibleRecordCase(attributes, name, fields))

    def apply[A](attributes: A, name: Name, fields: FieldT[A]*): Type[A] =
      Type(ExtensibleRecordCase(attributes, name, Chunk.fromIterable(fields)))

    def apply[A](attributes: A, name: String, fields: FieldT[A]*): Type[A] =
      Type(ExtensibleRecordCase(attributes, Name.fromString(name), Chunk.fromIterable(fields)))

    def apply(name: Name, fields: Field[UType]*): UType =
      Type(ExtensibleRecordCase((), name, Chunk.fromIterable(fields)))

    def apply(name: Name, fields: Chunk[Field[UType]]): UType =
      Type(ExtensibleRecordCase((), name, fields))

    def apply(name: String, fields: Field[UType]*): UType =
      Type(ExtensibleRecordCase((), Name.fromString(name), Chunk.fromIterable(fields)))

    def apply(name: String, fields: Chunk[Field[UType]]): UType =
      Type(ExtensibleRecordCase((), Name.fromString(name), fields))

    def unapply[A](self: Type[A]): Option[(A, Name, Chunk[FieldT[A]])] =
      self.caseValue match {
        case ExtensibleRecordCase(attributes, name, fields) => Some((attributes, name, fields))
        case _                                              => None
      }
  }

  object Function {
    def apply[A](attributes: A, argumentType: Type[A], returnType: Type[A]): Type[A] =
      Type(FunctionCase(attributes, argumentType, returnType))

    def apply(argumentType: UType, returnType: UType): UType = Type(FunctionCase((), argumentType, returnType))

    // def apply[A](attributes: A, paramTypes: Chunk[Type[A]], returnType: Type[A])(implicit
    //     ev: NeedsAttributes[A]
    // ): Type[A] =
    //   Type(FunctionCase(attributes, paramTypes, returnType))

    // def apply[A](attributes: A, paramTypes: Type[A]*)(returnType: Type[A])(implicit
    //     ev: NeedsAttributes[A]
    // ): Type[A] =
    //   Type(FunctionCase(attributes, Chunk.fromIterable(paramTypes), returnType))

    def unapply[A](self: Type[A]): Option[(A, Type[A], Type[A])] =
      self.caseValue match {
        case FunctionCase(attributes, argumentType, returnType) => Some((attributes, argumentType, returnType))
        case _                                                  => None
      }
  }

  object Record {

    val empty: UType = Type(RecordCase((), Chunk.empty))

    def apply[A](attributes: A): CreateAttributed[A] = new CreateAttributed(attributes)

    def apply(fields: Chunk[Field[UType]]): UType = Type(RecordCase((), fields))

    def apply(fields: FieldT[Any]*): UType = Type(RecordCase((), Chunk.fromIterable(fields)))

    def empty[A](attributes: A): Type[A] = Type(RecordCase(attributes, Chunk.empty))

    def unapply[A](self: Type[A]): Option[(A, Chunk[FieldT[A]])] =
      self.caseValue match {
        case RecordCase(attributes, fields) => Some((attributes, fields))
        case _                              => None
      }

    final class CreateAttributed[A](val attributes: A) extends AnyVal {
      def apply(fields: Chunk[FieldT[A]]): Type[A] = Type(RecordCase(attributes, fields))
      def apply(fields: FieldT[A]*): UType         = Type(RecordCase(attributes, Chunk.fromIterable(fields)))
    }
  }

  object Reference {
    def apply[A](attributes: A, name: FQName, typeParams: Chunk[Type[A]]): Type[A] =
      Type(ReferenceCase(attributes, name, typeParams))

    def apply[A](attributes: A, name: String, typeParams: Chunk[Type[A]]): Type[A] =
      Type(ReferenceCase(attributes, FQName.fromString(name), typeParams))

    def apply[A](attributes: A, name: FQName, typeParams: Type[A]*): Type[A] =
      Type(ReferenceCase(attributes, name, Chunk.fromIterable(typeParams)))

    def apply[A](attributes: A, name: String, typeParams: Type[A]*): Type[A] =
      Type(ReferenceCase(attributes, FQName.fromString(name), Chunk.fromIterable(typeParams)))

    def apply(name: FQName): ApplyGivenName = new ApplyGivenName(name)
    def apply(name: String): ApplyGivenName = new ApplyGivenName(FQName.fromString(name))

    def unapply[A](self: Type[A]): Option[(A, FQName, Chunk[Type[A]])] =
      self.caseValue match {
        case ReferenceCase(attributes, name, typeParams) => Some((attributes, name, typeParams))
        case _                                           => None
      }

    final class ApplyGivenName(val name: FQName) extends AnyVal {
      def apply(typeParams: UType*): UType = Type(ReferenceCase((), name, Chunk.fromIterable(typeParams)))
    }
  }

  object Tuple {
    def apply[A](attributes: A, elements: Chunk[Type[A]])(implicit ev: NeedsAttributes[A]): Type[A] =
      tuple(attributes, elements)

    def apply[A](attributes: A, elements: Type[A]*)(implicit ev: NeedsAttributes[A]): Type[A] =
      tuple(attributes, elements: _*)

    def withElements(elements: UType*): UType = Type(TupleCase((), Chunk.fromIterable(elements)))

    def unapply[A](t: Type[A]): Option[(A, Chunk[Type[A]])] =
      t.caseValue match {
        case TupleCase(attributes, elements) => Some(attributes -> elements)
        case _                               => None
      }
  }

  object Unit {
    def apply[A](attributes: A): Type[A] = unit(attributes)
    def unapply[A](self: Type[A]): Option[A] = self.caseValue match {
      case UnitCase(attributes) => Some(attributes)
      case _                    => None
    }
  }

  object Variable {
    def apply[A](attributes: A, name: String): Type[A] = variable(attributes, name)
    def apply[A](attributes: A, name: Name): Type[A]   = variable(attributes, name)
    def unapply[A](self: Type[A]): Option[(A, Name)] = self.caseValue match {
      case VariableCase(attributes, name) => Some(attributes -> name)
      case _                              => None
    }
  }
  implicit val CovariantType: Covariant[Type] = new Covariant[Type] {
    override def map[A, B](f: A => B): Type[A] => Type[B] = tpe => tpe.mapAttributes(f)
  }

  final class MapTypeAttributes[+A](val input: () => Type[A]) extends AnyVal {
    def apply[B](f: A => B): Type[B] = input().map(f)
  }

  implicit class UTypeExtensions(private val self: UType) extends AnyVal {
    def -->(that: UType): UType = Function(self, that)
  }
}
