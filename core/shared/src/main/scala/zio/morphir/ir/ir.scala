package zio.morphir.ir

import zio.Chunk

sealed trait Type { self =>
  import TypeCase.*

  final def asType: Type = self

  def $case: TypeCase[Type]

  final def fold[Z](f: TypeCase[Z] => Z): Z = self.$case match {
    case c @ ExtensibleRecordCase(_, _)           => f(ExtensibleRecordCase(c.name, c.fields.map(_.fold(f))))
    case c @ FieldCase(_, _)                      => f(FieldCase(c.name, c.fieldType.fold(f)))
    case c @ FunctionCase(paramTypes, returnType) => f(FunctionCase(paramTypes.map(_.fold(f)), returnType.fold(f)))
    case c @ RecordCase(_)                        => f(RecordCase(c.fields.map(_.fold(f))))
    case c @ ReferenceCase(typeName, typeParams)  => f(ReferenceCase(typeName, typeParams.map(_.fold(f))))
    case c @ TupleCase(_)                         => f(TupleCase(c.elementTypes.map(_.fold(f))))
    case c @ UnitCase                             => f(UnitCase)
    case c @ VariableCase(name)                   => f(c)
  }

  /**
   * Folds over the recursive data structure to reduce it to a summary value,
   * providing access to the recursive structure annotated with the current
   * previous summary values in each step of the fold.
   */
  def foldAttributed[Z](f: TypeCase[Attributed[TypeCase, Z]] => Z): Z = {
    def annotate(recursive: Type): Attributed[TypeCase, Z] =
      Attributed(recursive.$case.map(annotate), recursive.foldAttributed(f))
    f($case.map(annotate))
  }
}

object Type {
  import TypeCase.*

  def ref(name: naming.FQName): Reference = Reference(name, Chunk.empty)

  /**
   * Creates a type variable with the given `name`.
   */
  def variable(name: Name): Variable   = Variable(name)
  def variable(name: String): Variable = variable(Name(name))
  val unit: Type                       = UnitType

  case object UnitType extends Type {
    override val $case: TypeCase[Type] = UnitCase
  }

  final case class Field private ($case: FieldCase[Type]) extends Type
  object Field {
    def apply(name: Name, fieldType: Type): Field   =
      Field(FieldCase(name, fieldType))
    def unapply(field: Field): Option[(Name, Type)] =
      Some((field.$case.name, field.$case.fieldType))

    object Case {
      def unapply(field: Field): Option[FieldCase[Type]] =
        Some(field.$case)
    }
  }

  final case class Reference(name: FQName, typeParams: Chunk[Type]) extends Type {
    override lazy val $case: ReferenceCase[Type] = ReferenceCase(name, typeParams)
  }

  object Reference {
    object Case {
      def unapply(reference: Reference): Option[ReferenceCase[Type]] =
        Some(reference.$case)
    }
  }

  final case class Variable(name: Name) extends Type {
    override lazy val $case: VariableCase = VariableCase(name)
  }
  object Variable {
    object Case {
      def unapply(variable: Variable): Option[VariableCase] =
        Some(variable.$case)
    }
  }
}

sealed trait TypeCase[+A] { self =>
  import TypeCase.*
  def map[B](f: A => B): TypeCase[B] = self match {
    case c @ ExtensibleRecordCase(_, _) => ExtensibleRecordCase(c.name, c.fields.map(f))
    case c @ FieldCase(_, _)            => FieldCase(c.name, f(c.fieldType))
    case c @ FunctionCase(_, _)         => FunctionCase(c.paramTypes.map(f), f(c.returnType))
    case c @ ReferenceCase(_, _)        => ReferenceCase(c.typeName, c.typeParams.map(f))
    case c @ TupleCase(_)               => TupleCase(c.elementTypes.map(f))
    case c @ UnitCase                   => UnitCase
    case c @ VariableCase(_)            => VariableCase(c.name)
    case c @ RecordCase(_)              => RecordCase(c.fields.map(f))
  }
}

object TypeCase {
  final case class ExtensibleRecordCase[+A](name: Name, fields: Chunk[A])    extends TypeCase[A]
  final case class FunctionCase[+A](paramTypes: List[A], returnType: A)      extends TypeCase[A]
  final case class RecordCase[+A](fields: Chunk[A])                          extends TypeCase[A]
  final case class ReferenceCase[+A](typeName: FQName, typeParams: Chunk[A]) extends TypeCase[A]
  final case class TupleCase[+A](elementTypes: List[A])                      extends TypeCase[A]
  case object UnitCase                                                       extends TypeCase[Nothing]
  final case class VariableCase(name: Name)                                  extends TypeCase[Nothing]
  final case class FieldCase[+A](name: Name, fieldType: A)                   extends TypeCase[A]
}

final case class Attributed[Case[+_], A](caseValue: Case[Attributed[Case, A]], attributes: A)

// final case class Field[+A](name: Name, fieldType: TypeCase[A]) { self =>
//   def map[B](f: A => B): Field[B] = copy(fieldType = fieldType.map(f))
// }

// object Field {
//   implicit val FieldCovariant: Covariant[Field] = new Covariant[Field] {
//     def map[A, B](fa: Field[A])(f: A => B): Field[B] = fa.map(f)
//   }
// }

// final case class Fields[+A](items: zio.Chunk[Field[A]]) { self =>
//   def map[B](f: A => B): Fields[B] = copy(items = items.map(_.map(f)))
// }

// object Fields {
//   implicit val FieldsCovariant: Covariant[Fields] = new Covariant[Fields] {
//     def map[A, B](fa: Fields[A])(f: A => B): Fields[B] = fa.map(f)
//   }
// }
