package zio.morphir.ir

import zio.morphir.ir.{Literal => Lit}
import zio.{Chunk, ZEnvironment}
import zio.prelude.*

object ValueModule {

  sealed trait Value[+Annotations] { self =>
    import ValueCase.*

    def annotations: ZEnvironment[Annotations]
    def caseValue: ValueCase[Value[Annotations]]

    def fold[Z](f: ValueCase[Z] => Z): Z = self.caseValue match {

      case c @ PatternCase.AsPatternCase(_, _) => f(PatternCase.AsPatternCase(c.pattern.fold(f), c.name))
      case c @ PatternCase.ConstructorPatternCase(_, _) =>
        f(PatternCase.ConstructorPatternCase(c.constructorName, c.argumentPatterns.map(_.fold(f))))
      case _ @PatternCase.EmptyListPatternCase => f(PatternCase.EmptyListPatternCase)
      case c @ PatternCase.HeadTailPatternCase(_, _) =>
        f(PatternCase.HeadTailPatternCase(c.head.fold(f), c.tail.fold(f)))
      case c @ PatternCase.LiteralPatternCase(_) => f(PatternCase.LiteralPatternCase(c.value))
      case c @ PatternCase.TuplePatternCase(_)   => f(PatternCase.TuplePatternCase(c.elements.map(_.fold(f))))
      case _ @PatternCase.UnitPatternCase        => f(PatternCase.UnitPatternCase)
      case _ @PatternCase.WildcardPatternCase    => f(PatternCase.WildcardPatternCase)
      case c @ ValueCase.ApplyCase(_, _)    => f(ValueCase.ApplyCase(c.function.fold(f), c.arguments.map(_.fold(f))))
      case c @ ValueCase.ConstructorCase(_) => f(ValueCase.ConstructorCase(c.name))
      case c @ ValueCase.DestructureCase(_, _, _) =>
        f(ValueCase.DestructureCase(c.pattern.fold(f), c.valueToDestruct.fold(f), c.inValue.fold(f)))
      case c @ ValueCase.FieldCase(_, _)      => f(ValueCase.FieldCase(c.target.fold(f), c.name))
      case c @ ValueCase.FieldFunctionCase(_) => f(c)
      case c @ ValueCase.IfThenElseCase(_, _, _) =>
        f(ValueCase.IfThenElseCase(c.condition.fold(f), c.thenBranch.fold(f), c.elseBranch.fold(f)))
      case c @ ValueCase.LambdaCase(_, _) => f(ValueCase.LambdaCase(c.argumentPattern.fold(f), c.body.fold(f)))
      case c @ ValueCase.LetDefinitionCase(_, _, _) =>
        f(ValueCase.LetDefinitionCase(c.valueName, c.valueDefinition.fold(f), c.inValue.fold(f)))
      case c @ ValueCase.LetRecursionCase(_, _) =>
        f(
          ValueCase.LetRecursionCase(
            c.valueDefinitions.map { case (name, value) => (name, value.fold(f)) },
            c.inValue.fold(f)
          )
        )
      case c @ ValueCase.ListCase(_)           => f(ValueCase.ListCase(c.elements.map(_.fold(f))))
      case c @ ValueCase.LiteralCase(_)        => f(c)
      case c @ ValueCase.NativeApplyCase(_, _) => f(ValueCase.NativeApplyCase(c.function, c.arguments.map(_.fold(f))))
      case c @ ValueCase.PatternMatchCase(_, _) =>
        f(
          ValueCase.PatternMatchCase(
            c.branchOutOn.fold(f),
            c.cases.map { case (pattern, value) =>
              (pattern.fold(f), value.fold(f))
            }
          )
        )
      case c @ ValueCase.RecordCase(_)    => f(ValueCase.RecordCase(c.fields.map { case (k, v) => (k, v.fold(f)) }))
      case c @ ValueCase.ReferenceCase(_) => f(c)
      case c @ ValueCase.TupleCase(_)     => f(ValueCase.TupleCase(c.elements.map(_.fold(f))))
      case _ @ValueCase.UnitCase          => f(ValueCase.UnitCase)
      case c @ ValueCase.UpdateRecordCase(_, _) =>
        f(
          ValueCase.UpdateRecordCase(
            c.valueToUpdate.fold(f),
            c.fieldsToUpdate.map { case (name, value) => (name, value.fold(f)) }
          )
        )
      case c @ ValueCase.VariableCase(_) => f(c)
    }
  }

  object Value {
    import ValueCase.*

    final case class Literal[+V, +Annotations](value: Lit[V], annotations: ZEnvironment[Annotations])
        extends Value[Annotations] {
      def caseValue: LiteralCase = ValueCase.LiteralCase(value)
    }

    final case class Record[+Annotations](
        fields: Chunk[(Name, Value[Annotations])],
        annotations: ZEnvironment[Annotations]
    ) extends Value[Annotations] {
      override lazy val caseValue: ValueCase[Value[Annotations]] = RecordCase(fields)
    }

    final case class Unit[+Annotations](annotations: ZEnvironment[Annotations]) extends Value[Annotations] {
      override def caseValue: UnitCase = ValueCase.UnitCase
    }
  }

  sealed trait ValueCase[+Self] { self =>
    import ValueCase.*

    def map[B](f: Self => B): ValueCase[B] = self match {
      case c @ ApplyCase(_, _)          => ApplyCase(f(c.function), c.arguments.map(f))
      case c @ ConstructorCase(_)       => ConstructorCase(c.name)
      case c @ DestructureCase(_, _, _) => DestructureCase(f(c.pattern), f(c.valueToDestruct), f(c.inValue))
      case c @ FieldCase(_, _)          => FieldCase(f(c.target), c.name)
      case c @ FieldFunctionCase(_)     => FieldFunctionCase(c.name)
      case c @ IfThenElseCase(_, _, _) =>
        IfThenElseCase(f(c.condition), f(c.thenBranch), f(c.elseBranch))
      case c @ LambdaCase(_, _)           => LambdaCase(f(c.argumentPattern), f(c.body))
      case c @ LetDefinitionCase(_, _, _) => LetDefinitionCase(c.valueName, f(c.valueDefinition), f(c.inValue))
      case c @ LetRecursionCase(_, _) =>
        LetRecursionCase(c.valueDefinitions.map { case (name, value) => (name, f(value)) }, f(c.inValue))
      case c @ ListCase(_)           => ListCase(c.elements.map(f))
      case c @ LiteralCase(_)        => LiteralCase(c.literal)
      case c @ NativeApplyCase(_, _) => NativeApplyCase(c.function, c.arguments.map(f))
      case c @ PatternMatchCase(_, _) =>
        PatternMatchCase(f(c.branchOutOn), c.cases.map { case (p, v) => (f(p), f(v)) })
      case c @ RecordCase(_)    => RecordCase(c.fields.map { case (name, value) => (name, f(value)) })
      case c @ ReferenceCase(_) => c
      case c @ TupleCase(_)     => TupleCase(c.elements.map(f))
      case _ @UnitCase          => UnitCase
      case c @ UpdateRecordCase(_, _) =>
        UpdateRecordCase(f(c.valueToUpdate), c.fieldsToUpdate.map { case (name, self) => (name, f(self)) })
      case c @ VariableCase(_) => c
      case c: PatternCase[_]   => c.map(f)

    }
  }

  object ValueCase {
    final case class NativeApplyCase[+Self](function: NativeFunction, arguments: Chunk[Self]) extends ValueCase[Self]
    final case class ApplyCase[+Self](function: Self, arguments: List[Self])                  extends ValueCase[Self]
    final case class ConstructorCase(name: FQName)                                            extends ValueCase[Nothing]
    final case class FieldCase[+Self](target: Self, name: Name)                               extends ValueCase[Self]
    final case class FieldFunctionCase(name: Name)                                            extends ValueCase[Nothing]
    final case class IfThenElseCase[+Self](condition: Self, thenBranch: Self, elseBranch: Self) extends ValueCase[Self]
    final case class ListCase[+Self](elements: Chunk[Self])                                     extends ValueCase[Self]
    final case class LiteralCase(literal: LiteralValue)                                    extends ValueCase[Nothing]
    final case class PatternMatchCase[+Self](branchOutOn: Self, cases: List[(Self, Self)]) extends ValueCase[Self]
    final case class RecordCase[+Self](fields: Chunk[(Name, Self)])                        extends ValueCase[Self]
    final case class ReferenceCase(name: FQName)                                           extends ValueCase[Nothing]
    final case class TupleCase[+Self](elements: Chunk[Self])                               extends ValueCase[Self]
    case object UnitCase                                                                   extends ValueCase[Nothing]
    type UnitCase = UnitCase.type
    final case class VariableCase(name: Name) extends ValueCase[Nothing]
    final case class LetDefinitionCase[+Self](valueName: Name, valueDefinition: Self, inValue: Self)
        extends ValueCase[Self]
    final case class LetRecursionCase[+Self](valueDefinitions: Map[Name, Self], inValue: Self) extends ValueCase[Self]
    final case class UpdateRecordCase[+Self](valueToUpdate: Self, fieldsToUpdate: Chunk[(Name, Self)])
        extends ValueCase[Self]
    final case class LambdaCase[+Self](argumentPattern: Self, body: Self)                        extends ValueCase[Self]
    final case class DestructureCase[+Self](pattern: Self, valueToDestruct: Self, inValue: Self) extends ValueCase[Self]

    sealed trait PatternCase[+Self] extends ValueCase[Self] { self =>
      import PatternCase.*

      override def map[B](f: Self => B): PatternCase[B] = self match {
        case c @ AsPatternCase(_, _) => AsPatternCase(f(c.pattern), c.name)
        case c @ PatternCase.ConstructorPatternCase(_, _) =>
          PatternCase.ConstructorPatternCase(c.constructorName, c.argumentPatterns.map(f))
        case EmptyListPatternCase                  => EmptyListPatternCase
        case c @ HeadTailPatternCase(_, _)         => HeadTailPatternCase(f(c.head), f(c.tail))
        case c @ PatternCase.LiteralPatternCase(_) => c
        case c @ PatternCase.TuplePatternCase(_)   => PatternCase.TuplePatternCase(c.elements.map(f))
        case PatternCase.UnitPatternCase           => PatternCase.UnitPatternCase
        case WildcardPatternCase                   => WildcardPatternCase
      }

    }

    sealed trait Pattern[+Annotations] extends Value[Annotations] {
      def annotations: ZEnvironment[Annotations]
    }
    object Pattern {
      // val unit: UnitPattern[Any] = UnitPattern(ZEnvironment.empty)
      // def unit[Annotations](annotations: ZEnvironment[Annotations]): UnitPattern[Annotations] = UnitPattern(annotations)
      // val wildcard: Wildcard[Any] = Wildcard(ZEnvironment.empty)
      // def wildcard[Annotations](annotations: ZEnvironment[Annotations]): Wildcard[Annotations] = Wildcard(annotations)

      // final case class LiteralPattern[+Annotations, +Value](value: Lit[Value], annotations: ZEnvironment[Annotations])
      //     extends Pattern[Annotations]

      // final case class UnitPattern[+Annotations](annotations: ZEnvironment[Annotations]) extends Pattern[Annotations]
      // final case class Wildcard[+Annotations](annotations: ZEnvironment[Annotations])    extends Pattern[Annotations]
    }
    object PatternCase {

      final case class AsPatternCase[+Self](pattern: Self, name: Name) extends PatternCase[Self]
      final case class ConstructorPatternCase[+Self](constructorName: FQName, argumentPatterns: List[Self])
          extends PatternCase[Self]
      case object EmptyListPatternCase                                    extends PatternCase[Nothing]
      final case class HeadTailPatternCase[+Self](head: Self, tail: Self) extends PatternCase[Self]
      final case class LiteralPatternCase(value: Literal[Nothing])        extends PatternCase[Nothing]
      final case class TuplePatternCase[+Self](elements: List[Self])      extends PatternCase[Self]
      case object UnitPatternCase                                         extends PatternCase[Nothing]
      case object WildcardPatternCase                                     extends PatternCase[Nothing]

      implicit val PatternCaseForEach: ForEach[PatternCase] =
        new ForEach[PatternCase] {
          def forEach[G[+_]: IdentityBoth: Covariant, A, B](fa: PatternCase[A])(f: A => G[B]): G[PatternCase[B]] =
            fa match {
              case c @ AsPatternCase(_, _) => f(c.pattern).map(AsPatternCase(_, c.name))
              case c @ ConstructorPatternCase(_, _) =>
                c.argumentPatterns.forEach(f).map(ConstructorPatternCase(c.constructorName, _))
              case EmptyListPatternCase          => EmptyListPatternCase.succeed
              case c @ HeadTailPatternCase(_, _) => f(c.head).zipWith(f(c.tail))(HeadTailPatternCase(_, _))
              case c @ LiteralPatternCase(_)     => c.succeed
              case c @ TuplePatternCase(_)       => c.elements.forEach(f).map(TuplePatternCase(_))
              case UnitPatternCase               => UnitPatternCase.succeed
              case WildcardPatternCase           => WildcardPatternCase.succeed
            }
        }
    }

    implicit val ValueCaseCovariant: Covariant[ValueCase] = new Covariant[ValueCase] {
      def map[A, B](f: A => B): ValueCase[A] => ValueCase[B] = _.map(f)
    }

    implicit val ValueCaseForEach: ForEach[ValueCase] =
      new ForEach[ValueCase] {
        def forEach[G[+_]: IdentityBoth: Covariant, A, B](fa: ValueCase[A])(f: A => G[B]): G[ValueCase[B]] =
          fa match {
            case c @ ApplyCase(_, _)    => f(c.function).zipWith(c.arguments.forEach(f))(ApplyCase(_, _))
            case c @ ConstructorCase(_) => c.succeed
            case c @ DestructureCase(_, _, _) =>
              (f(c.pattern), f(c.valueToDestruct), f(c.inValue)).mapN(DestructureCase(_, _, _))
            case c @ FieldCase(_, _)      => f(c.target).map(FieldCase(_, c.name))
            case c @ FieldFunctionCase(_) => c.succeed
            case c @ IfThenElseCase(_, _, _) =>
              (f(c.condition), f(c.thenBranch), f(c.elseBranch)).mapN(IfThenElseCase(_, _, _))
            case c @ LambdaCase(_, _) => f(c.argumentPattern).zipWith(f(c.body))(LambdaCase(_, _))
            case c @ LetDefinitionCase(_, _, _) =>
              f(c.valueDefinition).zipWith(f(c.inValue))(LetDefinitionCase(c.valueName, _, _))
            case c @ LetRecursionCase(_, _) =>
              c.valueDefinitions.forEach(f).zipWith(f(c.inValue))(LetRecursionCase(_, _))
            case c @ ListCase(_)           => c.elements.forEach(f).map(ListCase(_))
            case c @ LiteralCase(_)        => c.succeed
            case c @ NativeApplyCase(_, _) => c.arguments.forEach(f).map(NativeApplyCase(c.function, _))
            case c @ PatternMatchCase(_, _) =>
              f(c.branchOutOn)
                .zipWith(c.cases.forEach { case (key, value) => f(key).zip(f(value)) })(PatternMatchCase(_, _))
            case c @ RecordCase(_) =>
              c.fields.forEach { case (key, value) => f(value).map(key -> _) }.map(RecordCase(_))
            case c @ ReferenceCase(_) => c.succeed
            case c @ TupleCase(_)     => c.elements.forEach(f).map(TupleCase(_))
            case _ @UnitCase          => UnitCase.succeed
            case c @ UpdateRecordCase(_, _) =>
              f(c.valueToUpdate).zipWith(c.fieldsToUpdate.forEach { case (name, self) => f(self).map(name -> _) })(
                UpdateRecordCase(_, _)
              )
            case c @ VariableCase(_) => c.succeed
            case c: PatternCase[_]   => c.forEach(f)
          }
      }
  }

}
