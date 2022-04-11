package zio.morphir.ir.value.recursive
import zio.Chunk
import zio.morphir.ir.value.Pattern
import zio.morphir.ir.{FQName, Literal, Name}
import zio.prelude._

sealed trait ValueCase[+TA, +VA, +Self] { self =>
  import ValueCase._
  def attributes: VA

  def map[Self2](f: Self => Self2): ValueCase[TA, VA, Self2] = self match {
    case c @ ApplyCase(_, _, _)          => ApplyCase(c.attributes, f(c.function), f(c.argument))
    case c @ ConstructorCase(_, _)       => c
    case c @ DestructureCase(_, _, _, _) => DestructureCase(c.attributes, c.pattern, f(c.valueToDestruct), f(c.inValue))
    case c @ FieldCase(_, _, _)          => FieldCase(c.attributes, f(c.target), c.name)
    case c @ FieldFunctionCase(_, _)     => c
    case c @ IfThenElseCase(_, _, _, _) =>
      IfThenElseCase(c.attributes, f(c.condition), f(c.thenBranch), f(c.elseBranch))
    case c @ LambdaCase(_, _, _) => LambdaCase(c.attributes, c.argumentPattern, f(c.body))
    case c @ LetDefinitionCase(_, _, _, _) =>
      LetDefinitionCase(c.attributes, c.valueName, c.valueDefinition.map(f), f(c.inValue))
    case c @ LetRecursionCase(_, _, _) =>
      LetRecursionCase(c.attributes, c.valueDefinitions.map { case (n, v) => (n, v.map(f)) }, f(c.inValue))
    case c @ ListCase(_, _)    => ListCase(c.attributes, c.elements.map(f))
    case c @ LiteralCase(_, _) => c
    case c @ PatternMatchCase(_, _, _) =>
      PatternMatchCase(c.attributes, f(c.branchOutOn), c.cases.map { case (p, v) => (p, f(v)) })
    case c @ RecordCase(_, _)    => RecordCase(c.attributes, c.fields.map { case (n, v) => (n, f(v)) })
    case c @ ReferenceCase(_, _) => c
    case c @ TupleCase(_, _)     => TupleCase(c.attributes, c.elements.map(f))
    case c @ UnitCase(_)         => c
    case c @ UpdateRecordCase(_, _, _) =>
      UpdateRecordCase(c.attributes, f(c.valueToUpdate), c.fieldsToUpdate.map { case (n, v) => (n, f(v)) })
    case c @ VariableCase(_, _) => c
  }

}

object ValueCase {
  final case class ApplyCase[+VA, +Self](
      attributes: VA,
      function: Self,
      argument: Self
  ) extends ValueCase[Nothing, VA, Self]

  final case class ConstructorCase[+VA](attributes: VA, name: FQName) extends ValueCase[Nothing, VA, Nothing]

  final case class DestructureCase[+VA, +Self](
      attributes: VA,
      pattern: Pattern[VA],
      valueToDestruct: Self,
      inValue: Self
  ) extends ValueCase[Nothing, VA, Self]

  final case class FieldCase[+VA, +Self](attributes: VA, target: Self, name: Name) extends ValueCase[Nothing, VA, Self]

  final case class FieldFunctionCase[+VA](attributes: VA, name: Name) extends ValueCase[Nothing, VA, Nothing]

  final case class IfThenElseCase[+VA, +Self](
      attributes: VA,
      condition: Self,
      thenBranch: Self,
      elseBranch: Self
  ) extends ValueCase[Nothing, VA, Self]

  final case class LambdaCase[+VA, +Self](attributes: VA, argumentPattern: Pattern[VA], body: Self)
      extends ValueCase[Nothing, VA, Self]

  final case class LetDefinitionCase[+TA, +VA, +TypeRepr[+_], +Self](
      attributes: VA,
      valueName: Name,
      valueDefinition: Definition.Case[TA, VA, TypeRepr, Self],
      inValue: Self
  ) extends ValueCase[TA, VA, Self]

  final case class LetRecursionCase[+TA, +VA, +TypeRepr[+_], +Self](
      attributes: VA,
      valueDefinitions: Map[Name, Definition.Case[TA, VA, TypeRepr, Self]],
      inValue: Self
  ) extends ValueCase[TA, VA, Self]

  final case class ListCase[+VA, +Self](attributes: VA, elements: Chunk[Self]) extends ValueCase[Nothing, VA, Self]

  final case class LiteralCase[+VA, +A](attributes: VA, literal: Literal[A]) extends ValueCase[Nothing, VA, Nothing]

  final case class PatternMatchCase[+VA, +Self](attributes: VA, branchOutOn: Self, cases: Chunk[(Pattern[VA], Self)])
      extends ValueCase[Nothing, VA, Self]

  final case class RecordCase[+VA, +Self](attributes: VA, fields: Chunk[(Name, Self)])
      extends ValueCase[Nothing, VA, Self]

  final case class ReferenceCase[+VA](attributes: VA, name: FQName) extends ValueCase[Nothing, VA, Nothing]

  final case class TupleCase[+VA, +Self](attributes: VA, elements: Chunk[Self]) extends ValueCase[Nothing, VA, Self]

  final case class UnitCase[+VA](attributes: VA) extends ValueCase[Nothing, VA, Nothing]

  final case class UpdateRecordCase[+VA, +Self](
      attributes: VA,
      valueToUpdate: Self,
      fieldsToUpdate: Chunk[(Name, Self)]
  ) extends ValueCase[Nothing, VA, Self]

  final case class VariableCase[+VA](attributes: VA, name: Name) extends ValueCase[Nothing, VA, Nothing]
//   implicit def ValueCaseCovariant[TA, VA]: Covariant[
//     ({ type ValueCasePartiallyApplied[Self] = ValueCase[TA, VA, Self] })#ValueCasePartiallyApplied
//   ] = {
//     type ValueCasePartiallyApplied[+Self] = ValueCase[TA, VA, Self]
//     new Covariant[ValueCasePartiallyApplied] {
//       def map[A, B](f: A => B): ValueCase[TA, VA, A] => ValueCase[TA, VA, B] = _.map(f)
//     }
//   }
//   implicit def ValueCaseForEach[TA, VA]: ForEach[({ type ValueCasePartiallyApplied[Self] = ValueCaseForEach[TA, VA, Self })#ValueCaseForEachPartiallyApplied] =
//   ??

  implicit def ValueCaseForEach[TA, VA]
      : ForEach[({ type ValueCasePartiallyApplied[+Self] = ValueCase[TA, VA, Self] })#ValueCasePartiallyApplied] = {
    type ValueCasePartiallyApplied[+Self] = ValueCase[TA, VA, Self]
    new ForEach[ValueCasePartiallyApplied] {
      def forEach[G[+_]: IdentityBoth: Covariant, A, B](fa: ValueCase[TA, VA, A])(
          f: A => G[B]
      ): G[ValueCase[TA, VA, B]] = fa match {
        case ApplyCase(attributes, function, argument)                          => ???
        case ConstructorCase(attributes, name)                                  => ???
        case DestructureCase(attributes, pattern, valueToDestruct, inValue)     => ???
        case FieldCase(attributes, target, name)                                => ???
        case FieldFunctionCase(attributes, name)                                => ???
        case IfThenElseCase(attributes, condition, thenBranch, elseBranch)      => ???
        case LambdaCase(attributes, argumentPattern, body)                      => ???
        case LetDefinitionCase(attributes, valueName, valueDefinition, inValue) => ???
        case LetRecursionCase(attributes, valueDefinitions, inValue)            => ???
        case ListCase(attributes, elements)                                     => ???
        case LiteralCase(attributes, literal)                                   => ???
        case PatternMatchCase(attributes, branchOutOn, cases)                   => ???
        case RecordCase(attributes, fields)                                     => ???
        case ReferenceCase(attributes, name)                                    => ???
        case TupleCase(attributes, elements)                                    => ???
        case UnitCase(attributes)                                               => ???
        case UpdateRecordCase(attributes, valueToUpdate, fieldsToUpdate)        => ???
        case VariableCase(attributes, name)                                     => ???
      }
    }
  }
}
