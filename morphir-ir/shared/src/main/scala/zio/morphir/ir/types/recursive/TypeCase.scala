package zio.morphir.ir.types.recursive
import zio.Chunk
import zio.morphir.ir._
import zio.prelude._

sealed trait TypeCase[+A, +Self] { self =>
  import TypeCase._

  def attributes: A

  final def map[Self2](f: Self => Self2): TypeCase[A, Self2] =
    self match {
      case c @ ExtensibleRecordCase(_, _, _) => ExtensibleRecordCase(c.attributes, c.name, c.fields.map(_.map(f)))
      case c @ FunctionCase(_, _, _)         => FunctionCase(c.attributes, c.paramTypes.map(f), f(c.returnType))
      case c @ RecordCase(_, _)              => RecordCase(c.attributes, c.fields.map(_.map(f)))
      case c @ ReferenceCase(_, _, _)        => ReferenceCase(c.attributes, c.typeName, c.typeParams.map(f))
      case c @ TupleCase(_, _)               => TupleCase(c.attributes, c.elements.map(f))
      case c @ UnitCase(_)                   => c
      case c @ VariableCase(_, _)            => c
    }

}

object TypeCase {
  final case class ExtensibleRecordCase[+A, +Self](attributes: A, name: Name, fields: Chunk[Field[Self]])
      extends TypeCase[A, Self]
  final case class FunctionCase[+A, +Self](attributes: A, paramTypes: Chunk[Self], returnType: Self)
      extends TypeCase[A, Self]
  final case class RecordCase[+A, +Self](attributes: A, fields: Chunk[Field[Self]]) extends TypeCase[A, Self]
  final case class ReferenceCase[+A, +Self](attributes: A, typeName: FQName, typeParams: Chunk[Self])
      extends TypeCase[A, Self]
  final case class TupleCase[+A, +Self](attributes: A, elements: Chunk[Self]) extends TypeCase[A, Self]
  final case class UnitCase[+A](attributes: A)                                extends TypeCase[A, Nothing]
  final case class VariableCase[+A](attributes: A, name: Name)                extends TypeCase[A, Nothing]

  implicit def TypeCaseForEach[Attributes]
      : ForEach[({ type TypeCasePartiallyApplied[+Self] = TypeCase[Attributes, Self] })#TypeCasePartiallyApplied] = {
    type TypeCasePartiallyApplied[+Self] = TypeCase[Attributes, Self]
    new ForEach[TypeCasePartiallyApplied] {
      def forEach[G[+_]: IdentityBoth: Covariant, A, B](
          fa: TypeCase[Attributes, A]
      )(f: A => G[B]): G[TypeCase[Attributes, B]] =
        fa match {
          case ExtensibleRecordCase(attributes, name, fields) =>
            fields.forEach(_.forEach(f)).map(fields => ExtensibleRecordCase(attributes, name, fields))
          case FunctionCase(attributes, paramTypes, returnType) =>
            paramTypes
              .forEach(f)
              .zipWith(f(returnType))((paramTypes, returnType) => FunctionCase(attributes, paramTypes, returnType))
          case RecordCase(attributes, fields) =>
            fields.forEach(_.forEach(f)).map(fields => RecordCase(attributes, fields))
          case ReferenceCase(attributes, typeName, typeParams) =>
            typeParams.forEach(f).map(typeParams => ReferenceCase(attributes, typeName, typeParams))
          case TupleCase(attributes, elementTypes) =>
            elementTypes.forEach(f).map(elementTypes => TupleCase(attributes, elementTypes))
          case UnitCase(attributes)           => UnitCase(attributes).succeed
          case VariableCase(attributes, name) => VariableCase(attributes, name).succeed
        }
    }
  }

}

// trait TypeMaker[A, Self] {
//   def make(caseValue: TypeCase[A, Self]): TypeInstance
// }
