package zio.morphir.ir.value
import zio.Chunk
import zio.morphir.ir.{FQName, Literal, Name}

sealed trait ValueCase[+TA, +VA, +Self] {
  def attributes: VA
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

//   implicit def ValueCaseForEach[TA, VA]: ForEach[({ type ValueCasePartiallyApplied[Self] = ValueCaseForEach[TA, VA, Self })#ValueCaseForEachPartiallyApplied] =
//   ???
}

final case class ValueExpr[+TA, +VA](caseValue: ValueCase[TA, VA, ValueExpr[TA, VA]]) { self =>
  def attributes: VA = caseValue.attributes
}
object ValueExpr {
  import ValueCase._
  def apply[TA, VA](attributes: VA, function: ValueExpr[TA, VA], argument: ValueExpr[TA, VA]): ValueExpr[TA, VA] =
    ValueExpr(ApplyCase(attributes, function, argument))
}
