package zio.morphir.ir

import zio.morphir.ir.{Literal => Lit}
import zio.morphir.ir.TypeModule.Type
import zio.{Chunk, ZEnvironment, ZIO}
import zio.prelude.*
import zio.prelude.fx.ZPure
import zio.morphir.syntax.ValueSyntax

object ValueModule {

  def mapDefinition[Annotations, Err](definition: Definition[Annotations])(
      tryMapType: Type[Annotations] => Validation[Err, Type[Annotations]],
      tryMapValue: Value[Annotations] => Validation[Err, Value[Annotations]]
  ): Validation[Err, Definition[Annotations]] = ???

  def mapSpecificationAttributes[A, B](spec: Specification[A])(func: A => B): Specification[B] =
    spec.mapSpecificationAttributes(func)

//  def mapValueAttributes[A, B](value: Value[A])(func: A => B): Value[B] = ???

//  def mapPatternAttributes[A, B](pattern: Pattern[A])(func: A => B): Pattern[B] = ???

//  def mapDefinitionAttributes[A, B](definition: Definition[A])(func1: A => B): Pattern[B] = ???

//  def collectValueAttributes[Annotations](value: Value[Annotations]): List[Annotations] = ???

//  def countValueNodes[Annotations](value: Value[Annotations]): Int = ???

//  def collectPatternAttributes[Annotations](pattern: Pattern[Annotations]): List[Annotations] = ???

//  def collectDefinitionAttributes[Annotations](definition: Definition[Annotations]): List[Annotations] = ???

  def collectVariables[Annotations](value: Value[Annotations]): Set[Name] = value.collectVariables

  def collectReferences[Annotations](value: Value[Annotations]): Set[FQName] = value.collectReferences

  def collectPatternVariables[Annotations](pattern: Pattern[Annotations]): Set[Name] = ???

  def collectPatternReferences[Annotations](pattern: Pattern[Annotations]): Set[FQName] = ???

  def toRawValue[Annotations](value: Value[Annotations]): RawValue = ???

  final case class Definition[+Annotations](
      inputTypes: Chunk[InputParameter[Annotations]],
      outputType: Type[Annotations],
      body: Value[Annotations]
  ) { self =>
    final def toSpecification: Specification[Annotations] =
      Specification(inputTypes.map(input => (input.name, input.tpe)), outputType)

    final def toValue: Value[Annotations] = {
      inputTypes.toList match {
        case Nil => body
        case inputParam :: restOfArgs =>
          inputParam.toValue(Definition(Chunk.fromIterable(restOfArgs), outputType, body).toValue)
      }
    }

    def collectAttributes: List[Annotations] = ???

    def transform[Annotations2 >: Annotations, Err](
        tryMapType: Type[Annotations2] => Validation[Err, Type[Annotations2]],
        tryMapValue: Value[Annotations2] => Validation[Err, Value[Annotations2]]
    ): Validation[Err, Definition[Annotations2]] = {
      ???
    }
    // f(outputType).map(outputType => self.copy(outputType = outputType))
  }

  final case class InputParameter[+Annotations](
      name: Name,
      tpe: Type[Annotations],
      annotations: ZEnvironment[Annotations]
  ) {
    def toValue[A >: Annotations](body: Value[A]): Value[A] =
      Value.Lambda[A](
        Pattern.AsPattern[A](Pattern.Wildcard[A](annotations), name, annotations),
        body,
        annotations
      )
  }

  final type RawValue = Value[Any]
  final val RawValue = Value

  final case class Specification[+Annotations](inputs: Chunk[(Name, Type[Annotations])], output: Type[Annotations]) {
    def mapSpecificationAttributes[B](func: Annotations => B): Specification[B] = ???
  }

  final type TypedValue = Value[UType]
  val TypedValue = Value

  sealed trait Pattern[+Annotations] extends Value[Annotations] {
    def annotations: ZEnvironment[Annotations]
    def mapAttributes[B](f: Annotations => B): Pattern[B] = ???
  }

  object Pattern {
    import ValueCase.*

    import ValueCase.PatternCase.*
    // val unit: UnitPattern[Any] = UnitPattern(ZEnvironment.empty)
    // def unit[Annotations](annotations: ZEnvironment[Annotations]): UnitPattern[Annotations] = UnitPattern(annotations)
    // val wildcard: Wildcard[Any] = Wildcard(ZEnvironment.empty)
    // def wildcard[Annotations](annotations: ZEnvironment[Annotations]): Wildcard[Annotations] = Wildcard(annotations)

    // final case class LiteralPattern[+Annotations, +Value](value: Lit[Value], annotations: ZEnvironment[Annotations])
    //     extends Pattern[Annotations]

    final case class UnitPattern[+Annotations](annotations: ZEnvironment[Annotations]) extends Pattern[Annotations] {
      override def caseValue: UnitPatternCase = UnitPatternCase
    }

    final case class Wildcard[+Annotations](annotations: ZEnvironment[Annotations]) extends Pattern[Annotations] {
      override def caseValue: WildcardPatternCase = WildcardPatternCase
    }

    final case class AsPattern[+Annotations](
        pattern: Pattern[Annotations],
        name: Name,
        annotations: ZEnvironment[Annotations]
    ) extends Pattern[Annotations] {
      override def caseValue: PatternCase[Pattern[Annotations]] = AsPatternCase(pattern, name)
    }

    object AsPattern {
      object Case {
        def apply(
            caseValue: AsPatternCase[Pattern[Any]]
        ): AsPattern[Any] =
          AsPattern(caseValue.pattern, caseValue.name, ZEnvironment.empty)

        def apply[Annotations](
            caseValue: AsPatternCase[Pattern[Annotations]],
            annotations: ZEnvironment[Annotations]
        ): AsPattern[Annotations] =
          AsPattern(caseValue.pattern, caseValue.name, annotations)
      }

      def unapply(pattern: Value[Any]): Option[(Pattern[Any], Name)] =
        pattern match {
          case AsPattern(pattern, name) => Some((pattern, name))
          case _                        => None
        }
    }

    final case class TuplePattern[+Annotations](
        elementPatterns: Chunk[Pattern[Annotations]],
        annotations: ZEnvironment[Annotations]
    ) extends Pattern[Annotations] {
      override def caseValue: TuplePatternCase[Pattern[Annotations]] = TuplePatternCase(elementPatterns)

    }

    final case class ConstructorPattern[+Annotations](
        constructorName: FQName,
        argumentPatterns: Chunk[Pattern[Annotations]],
        annotations: ZEnvironment[Annotations]
    ) extends Pattern[Annotations] {
      override def caseValue: ConstructorPatternCase[Pattern[Annotations]] =
        ConstructorPatternCase(constructorName, argumentPatterns)
    }

    final case class EmptyListPattern[+Annotations](annotations: ZEnvironment[Annotations])
        extends Pattern[Annotations] {
      override def caseValue: EmptyListPatternCase = EmptyListPatternCase
    }

    final case class HeadTailPattern[+Annotations](
        headPattern: Pattern[Annotations],
        tailPattern: Pattern[Annotations],
        annotations: ZEnvironment[Annotations]
    ) extends Pattern[Annotations] {
      override def caseValue: HeadTailPatternCase[Pattern[Annotations]] =
        HeadTailPatternCase(headPattern, tailPattern)
    }

    final case class LiteralPattern[+Annotations](
        literal: LiteralValue,
        annotations: ZEnvironment[Annotations]
    ) extends Pattern[Annotations] {
      override def caseValue: LiteralPatternCase =
        LiteralPatternCase(literal)
    }
  }

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

    def foldDown[Z](z: Z)(f: (Z, Value[Annotations]) => Z): Z =
      caseValue.foldLeft(f(z, self))((z, recursive) => recursive.foldDown(z)(f))

    def foldDownSome[Z](z: Z)(pf: PartialFunction[(Z, Value[Annotations]), Z]): Z =
      foldDown(z)((z, recursive) => pf.lift(z -> recursive).getOrElse(z))

    def foldM[F[+_]: AssociativeFlatten: Covariant: IdentityBoth, Z](f: ValueCase[Z] => F[Z]): F[Z] =
      fold[F[Z]](_.flip.flatMap(f))

    def foldPure[W, S, R, E, Z](f: ValueCase[Z] => ZPure[W, S, S, R, E, Z]): ZPure[W, S, S, R, E, Z] =
      foldM(f)

    // TODO: Uncomment once appropriate instances are provided by ZIO Prelude

    // def foldManaged[R, E, Z](f: ValueCase[Z] => ZManaged[R, E, Z]): ZManaged[R, E, Z] =
    //   foldM(f)

    // def foldSTM[R, E, Z](f: ValueCase[Z] => ZSTM[R, E, Z]): ZSTM[R, E, Z] =
    //   foldM(f)

    // def foldValidation[W, E, Z](f: ValueCase[Z] => ZValidation[W, E, Z]): ZValidation[W, E, Z] =
    //   foldM(f)

    def foldZIO[R, E, Z](f: ValueCase[Z] => ZIO[R, E, Z]): ZIO[R, E, Z] =
      foldM(f)

    def foldRecursive[Z](f: ValueCase[(Value[Annotations], Z)] => Z): Z =
      f(caseValue.map(recursive => recursive -> recursive.foldRecursive(f)))

    def foldUp[Z](z: Z)(f: (Z, Value[Annotations]) => Z): Z =
      f(caseValue.foldLeft(z)((z, recursive) => recursive.foldUp(z)(f)), self)

    def foldUpSome[Z](z: Z)(pf: PartialFunction[(Z, Value[Annotations]), Z]): Z =
      foldUp(z)((z, recursive) => pf.lift(z -> recursive).getOrElse(z))

    // def toRawValue: RawValue = fold[RawValue] {
//    case c @ PatternCase.AsPatternCase(_, _)          => ???
//    case c @ PatternCase.ConstructorPatternCase(_, _) => ???
//    case _ @PatternCase.EmptyListPatternCase          => ???
//    case c @ PatternCase.HeadTailPatternCase(_, _)    => ???
//    case c @ PatternCase.LiteralPatternCase(_)        => ???
//    case c @ PatternCase.TuplePatternCase(_)          => ???
//    case _ @PatternCase.UnitPatternCase               => ???
//    case _ @PatternCase.WildcardPatternCase           => ???
//    case c @ ValueCase.ApplyCase(_, _)                => ???
//    case c @ ValueCase.ConstructorCase(_)             => ???
//    case c @ ValueCase.DestructureCase(_, _, _)       => ???
//    case c @ ValueCase.FieldCase(_, _)                => ???
//    case c @ ValueCase.FieldFunctionCase(_)           => ???
//    case c @ ValueCase.IfThenElseCase(_, _, _)        => ???
//    case c @ ValueCase.LambdaCase(_, _)               => ???
//    case c @ ValueCase.LetDefinitionCase(_, _, _)     => ???
//    case c @ ValueCase.LetRecursionCase(_, _)         => ???
//    case c @ ValueCase.ListCase(_)                    => ???
//    case c @ ValueCase.LiteralCase(_)                 => ???
//    case c @ ValueCase.NativeApplyCase(_, _)          => ???
//    case c @ ValueCase.PatternMatchCase(_, _) => ???
//    case c @ ValueCase.ReferenceCase(_)       => ???
//    case c @ ValueCase.RecordCase(_)          => ???
//    case c @ ValueCase.TupleCase(_)           => ???
//    case _ @ValueCase.UnitCase                => ???
//    case c @ ValueCase.UpdateRecordCase(_, _) => ???
//    case c @ ValueCase.VariableCase(_)        => ???
    // }

    def toRawValue: RawValue = fold[RawValue] {
      case c @ PatternCase.AsPatternCase(_, _)          => ???
      case c @ PatternCase.ConstructorPatternCase(_, _) => ???
      case _ @PatternCase.EmptyListPatternCase          => ???
      case c @ PatternCase.HeadTailPatternCase(_, _)    => ???
      case c @ PatternCase.LiteralPatternCase(_)        => ???
      case c @ PatternCase.TuplePatternCase(_)          => ???
      case _ @PatternCase.UnitPatternCase               => ???
      case _ @PatternCase.WildcardPatternCase           => ???
      case c @ ValueCase.ApplyCase(_, _)                => Value.Apply(c.function, c.arguments, ZEnvironment.empty)
      case c @ ValueCase.ConstructorCase(_)             => ???
      case c @ ValueCase.DestructureCase(_, _, _)       => ???
      case c @ ValueCase.FieldCase(_, _)                => Value.Field(c.target, c.name, ZEnvironment.empty)
      case c @ ValueCase.FieldFunctionCase(_)           => Value.FieldFunction(c.name, ZEnvironment.empty)
      case c @ ValueCase.IfThenElseCase(_, _, _)        => ???
      case c @ ValueCase.LambdaCase(_, _)               => Value.Lambda(c.argumentPattern, c.body, ZEnvironment.empty)
      case c @ ValueCase.LetDefinitionCase(_, _, _)     => ???
      case c @ ValueCase.LetRecursionCase(_, _)         => ???
      case c @ ValueCase.ListCase(_)                    => ???
      case c @ ValueCase.LiteralCase(_)                 => Value.Literal(c.literal, ZEnvironment.empty)
      case c @ ValueCase.NativeApplyCase(_, _)          => ???
      case c @ ValueCase.PatternMatchCase(_, _) => Value.PatternMatch(c.branchOutOn, c.cases, ZEnvironment.empty)
      case c @ ValueCase.ReferenceCase(_)       => ???
      case c @ ValueCase.RecordCase(_)          => ???
      case c @ ValueCase.TupleCase(_)           => ???
      case _ @ValueCase.UnitCase                => ???
      case c @ ValueCase.UpdateRecordCase(_, _) => ???
      case c @ ValueCase.VariableCase(_)        => ???
    }

    def transformDown[Annotations0 >: Annotations](
        f: Value[Annotations0] => Value[Annotations0]
    ): Value[Annotations0] = {
      def loop(recursive: Value[Annotations0]): Value[Annotations] =
        Value(f(recursive).caseValue.map(loop), annotations)
      loop(self)
    }

    def transformDownSome[Annotations0 >: Annotations](
        pf: PartialFunction[Value[Annotations0], Value[Annotations0]]
    ): Value[Annotations0] =
      transformDown[Annotations0]((recursive => pf.lift(recursive).getOrElse(recursive)))

    def transformUp[Annotations0 >: Annotations](
        f: Value[Annotations0] => Value[Annotations0]
    ): Value[Annotations0] = {
      def loop(recursive: Value[Annotations0]): Value[Annotations0] =
        f(Value(recursive.caseValue.map(loop), annotations))
      loop(self)
    }

    def transformUpSome[Annotations0 >: Annotations](
        pf: PartialFunction[Value[Annotations0], Value[Annotations0]]
    ): Value[Annotations0] =
      transformUp[Annotations0]((recursive => pf.lift(recursive).getOrElse(recursive)))

//    def collectAttributes: List[Annotations] = ???

//    def countValueNodes: Int = self.collectAttributes.length

    def collectVariables: Set[Name] = fold[Set[Name]] {
      case c @ PatternCase.AsPatternCase(_, _)          => c.pattern + c.name
      case c @ PatternCase.ConstructorPatternCase(_, _) => c.argumentPatterns.flatten.toSet
      case _ @PatternCase.EmptyListPatternCase          => Set.empty
      case c @ PatternCase.HeadTailPatternCase(_, _)    => c.head ++ c.tail
      case c @ PatternCase.LiteralPatternCase(_)        => Set.empty
      case c @ PatternCase.TuplePatternCase(_)          => c.elements.flatten.toSet
      case _ @PatternCase.UnitPatternCase               => Set.empty
      case _ @PatternCase.WildcardPatternCase           => Set.empty
      case c @ ValueCase.ApplyCase(_, _)                => c.function ++ c.arguments.flatten
      case c @ ValueCase.DestructureCase(_, _, _)       => c.valueToDestruct ++ c.inValue
      case c @ ValueCase.FieldCase(_, _)                => c.target
      case c @ ValueCase.FieldFunctionCase(_)           => Set(c.name)
      case c @ ValueCase.IfThenElseCase(_, _, _)        => c.condition ++ c.thenBranch ++ c.elseBranch
      case c @ ValueCase.LambdaCase(_, _)               => c.argumentPattern ++ c.body
      case c @ ValueCase.LetDefinitionCase(_, _, _)     => c.valueDefinition ++ c.inValue + c.valueName
      case c @ ValueCase.LetRecursionCase(_, _) =>
        c.valueDefinitions.foldLeft(Set.empty[Name])((acc, kv) => acc ++ kv._2 + kv._1)
      case c @ ValueCase.ListCase(_)            => c.elements.flatten.toSet
      case _ @ValueCase.LiteralCase(_)          => Set.empty
      case c @ ValueCase.PatternMatchCase(_, _) => c.cases.flatMap(_._2).toSet ++ c.branchOutOn
      case c @ ValueCase.RecordCase(_)          => c.fields.flatMap(_._2).toSet
      case c @ ValueCase.TupleCase(_)           => c.elements.flatten.toSet
      case _ @ValueCase.UnitCase                => Set.empty
      case c @ ValueCase.UpdateRecordCase(_, _) => c.fieldsToUpdate.flatMap(_._2).toSet ++ c.valueToUpdate
      case c @ ValueCase.VariableCase(_)        => Set(c.name)
      case _ => Set.empty // TODO: Ensure we actually want empty in the all these cases tests will help
    }

    def collectReferences: Set[FQName] = fold[Set[FQName]] {
      case c @ PatternCase.AsPatternCase(_, _)          => c.pattern
      case c @ PatternCase.ConstructorPatternCase(_, _) => c.argumentPatterns.flatten.toSet + c.constructorName
      case _ @PatternCase.EmptyListPatternCase          => Set.empty
      case c @ PatternCase.HeadTailPatternCase(_, _)    => c.head ++ c.tail
      case c @ PatternCase.LiteralPatternCase(_)        => Set.empty
      case c @ PatternCase.TuplePatternCase(_)          => c.elements.flatten.toSet
      case _ @PatternCase.UnitPatternCase               => Set.empty
      case _ @PatternCase.WildcardPatternCase           => Set.empty
      case c @ ValueCase.ApplyCase(_, _)                => c.function ++ c.arguments.flatten
      case c @ ValueCase.DestructureCase(_, _, _)       => c.valueToDestruct ++ c.inValue
      case c @ ValueCase.FieldCase(_, _)                => c.target
      case _ @ValueCase.FieldFunctionCase(_)            => Set.empty
      case c @ ValueCase.IfThenElseCase(_, _, _)        => c.condition ++ c.thenBranch ++ c.elseBranch
      case c @ ValueCase.LambdaCase(_, _)               => c.body
      case c @ ValueCase.LetDefinitionCase(_, _, _)     => c.valueDefinition ++ c.inValue
      case c @ ValueCase.LetRecursionCase(_, _) =>
        c.valueDefinitions.foldLeft(Set.empty[FQName])((acc, kv) => acc ++ kv._2)
      case c @ ValueCase.ListCase(_)            => c.elements.flatten.toSet
      case _ @ValueCase.LiteralCase(_)          => Set.empty
      case c @ ValueCase.PatternMatchCase(_, _) => c.cases.flatMap(_._2).toSet ++ c.branchOutOn
      case c @ ValueCase.RecordCase(_)          => c.fields.flatMap(_._2).toSet
      case c @ ValueCase.TupleCase(_)           => c.elements.flatten.toSet
      case _ @ValueCase.UnitCase                => Set.empty
      case c @ ValueCase.UpdateRecordCase(_, _) => c.fieldsToUpdate.flatMap(_._2).toSet ++ c.valueToUpdate
      case _ @ValueCase.VariableCase(_)         => Set.empty
      case _ => Set.empty // TODO: Ensure we actually want empty in the all these cases tests will help
    }

    // todo maybe implement indexedMapValue
  }

  object Value extends ValueSyntax {
    import ValueCase.*

    def apply(caseValue: ValueCase[Value[Any]]): Value[Any] = GenericValue(caseValue, ZEnvironment.empty)

    def apply[Annotations](
        caseValue: ValueCase[Value[Annotations]],
        annotations: ZEnvironment[Annotations]
    ): Value[Annotations] = GenericValue(caseValue, annotations)

    private final case class GenericValue[+Annotations](
        caseValue: ValueCase[Value[Annotations]],
        annotations: ZEnvironment[Annotations]
    ) extends Value[Annotations]

    final case class Apply[+Annotations] private[morphir] (
        function: Value[Annotations],
        arguments: Chunk[Value[Annotations]],
        annotations: ZEnvironment[Annotations]
    ) extends Value[Annotations] {
      override def caseValue: ApplyCase[Value[Annotations]] = ValueCase.ApplyCase(function, arguments)
    }

    object Apply {
      object Case {
        def apply(caseValue: ValueCase.ApplyCase[Value[Any]]): Apply[Any] =
          Apply(caseValue.function, caseValue.arguments, ZEnvironment.empty)
        def apply[Annotations](
            caseValue: ValueCase.ApplyCase[Value[Annotations]],
            annotations: ZEnvironment[Annotations]
        ): Apply[Annotations] =
          Apply(caseValue.function, caseValue.arguments, annotations)
      }
    }

    final case class Constructor[+Annotations] private[morphir] (
        name: FQName,
        annotations: ZEnvironment[Annotations]
    ) extends Value[Annotations] {
      override def caseValue: ConstructorCase = ValueCase.ConstructorCase(name)
    }

    object Constructor {
      object Case {
        def apply(caseValue: ValueCase.ConstructorCase): Constructor[Any] =
          Constructor(caseValue.name, ZEnvironment.empty)

        def apply[Annotations](
            caseValue: ValueCase.ConstructorCase,
            annotations: ZEnvironment[Annotations]
        ): Constructor[Annotations] =
          Constructor(caseValue.name, annotations)
      }
    }

    final case class Field[+Annotations] private[morphir] (
        target: Value[Annotations],
        name: Name,
        annotations: ZEnvironment[Annotations]
    ) extends Value[Annotations] {
      override lazy val caseValue: ValueCase[Value[Annotations]] = FieldCase(target, name)
    }

    object Field {
      object Case {
        def apply(caseValue: ValueCase.FieldCase[Value[Any]]): Field[Any] =
          Field(caseValue.target, caseValue.name, ZEnvironment.empty)
        def apply[Annotations](
            caseValue: ValueCase.FieldCase[Value[Annotations]],
            annotations: ZEnvironment[Annotations]
        ): Field[Annotations] =
          Field(caseValue.target, caseValue.name, annotations)
      }
    }

    final case class FieldFunction[+Annotations] private[morphir] (
        fieldName: Name,
        annotations: ZEnvironment[Annotations]
    ) extends Value[Annotations] {
      override lazy val caseValue: ValueCase[Value[Annotations]] = FieldFunctionCase(fieldName)
    }

    object FieldFunction {
      object Case {
        def apply(caseValue: ValueCase.FieldFunctionCase): FieldFunction[Any] =
          FieldFunction(caseValue.name, ZEnvironment.empty)
        def apply[Annotations](
            caseValue: ValueCase.FieldFunctionCase,
            annotations: ZEnvironment[Annotations]
        ): FieldFunction[Annotations] =
          FieldFunction(caseValue.name, annotations)
      }
    }

    final case class Lambda[+Annotations] private[morphir] (
        pattern: Value[Annotations], // TODO: Restrict to pattern only
        body: Value[Annotations],
        annotations: ZEnvironment[Annotations]
    ) extends Value[Annotations] {
      override lazy val caseValue: LambdaCase[Value[Annotations]] = LambdaCase(pattern, body)
    }

    object Lambda {
      object Case {
        def apply(caseValue: ValueCase.LambdaCase[Value[Any]]): Lambda[Any] =
          Lambda(caseValue.argumentPattern, caseValue.body, ZEnvironment.empty)
        def apply[Annotations](
            caseValue: ValueCase.LambdaCase[Value[Annotations]],
            annotations: ZEnvironment[Annotations]
        ): Lambda[Annotations] =
          Lambda(caseValue.argumentPattern, caseValue.body, annotations)
      }
    }

    final case class Literal[+A, Annotations] private[morphir] (value: Lit[A], annotations: ZEnvironment[Annotations])
        extends Value[Annotations] {
      def caseValue: LiteralCase[A] = ValueCase.LiteralCase(value)
    }

    object Literal {
      object Case {
        def apply[A](caseValue: ValueCase.LiteralCase[A]): Literal[A, Any] =
          Literal(caseValue.literal, ZEnvironment.empty)
      }
    }

    final case class PatternMatch[Annotations] private[morphir] (
        scrutinee: Value[Annotations],
        cases: Chunk[(Value[Annotations], Value[Annotations])],
        annotations: ZEnvironment[Annotations]
    ) extends Value[Annotations] {
      override lazy val caseValue: ValueCase[Value[Annotations]] = PatternMatchCase(scrutinee, cases)
    }

    object PatternMatch {
      object Case {
        def apply(caseValue: ValueCase.PatternMatchCase[Value[Any]]): PatternMatch[Any] =
          PatternMatch(caseValue.branchOutOn, caseValue.cases, ZEnvironment.empty)
        def apply[Annotations](
            caseValue: ValueCase.PatternMatchCase[Value[Annotations]],
            annotations: ZEnvironment[Annotations]
        ): PatternMatch[Annotations] =
          PatternMatch(caseValue.branchOutOn, caseValue.cases, annotations)
      }
    }

    final case class Record[Annotations] private[morphir] (
        fields: Chunk[(Name, Value[Annotations])],
        annotations: ZEnvironment[Annotations]
    ) extends Value[Annotations] {
      override lazy val caseValue: ValueCase[Value[Annotations]] = RecordCase(fields)
    }

    object Record {
      object Case {
        def apply(caseValue: ValueCase.RecordCase[Value[Any]]): Record[Any] =
          Record(caseValue.fields, ZEnvironment.empty)
        def apply[Annotations](
            caseValue: ValueCase.RecordCase[Value[Annotations]],
            annotations: ZEnvironment[Annotations]
        ): Record[Annotations] =
          Record(caseValue.fields, annotations)
      }
    }

    final case class Unit[Annotations] private[morphir] (annotations: ZEnvironment[Annotations])
        extends Value[Annotations] {
      override def caseValue: UnitCase = ValueCase.UnitCase
    }

    object Unit {
      object Case {
        def apply: Unit[Any] =
          Unit(ZEnvironment.empty)
      }
    }

    final case class Variable[Annotations] private[morphir] (name: Name, annotations: ZEnvironment[Annotations])
        extends Value[Annotations] {
      override lazy val caseValue: ValueCase[Value[Annotations]] = VariableCase(name)
    }

    object Variable {
      object Case {
        def apply(caseValue: ValueCase.VariableCase): Variable[Any] =
          Variable(caseValue.name, ZEnvironment.empty)
        def apply[Annotations](
            caseValue: ValueCase.VariableCase,
            annotations: ZEnvironment[Annotations]
        ): Variable[Annotations] =
          Variable(caseValue.name, annotations)
      }
    }

    final case class Tuple[Annotations] private[morphir] (
        elements: Chunk[Value[Annotations]],
        annotations: ZEnvironment[Annotations]
    ) extends Value[Annotations] {
      override val caseValue: ValueCase[Value[Annotations]] = TupleCase(elements)
    }

    object Tuple {
      object Case {
        def apply(caseValue: ValueCase.TupleCase[Value[Any]]): Tuple[Any] =
          Tuple(caseValue.elements, ZEnvironment.empty)
        def apply[Annotations](
            caseValue: ValueCase.TupleCase[Value[Annotations]],
            annotations: ZEnvironment[Annotations]
        ): Tuple[Annotations] =
          Tuple(caseValue.elements, annotations)
      }
    }

    final case class List[Annotations] private[morphir] (
        elements: Chunk[Value[Annotations]],
        annotations: ZEnvironment[Annotations]
    ) extends Value[Annotations] {
      override val caseValue: ValueCase[Value[Annotations]] = ListCase(elements)
    }

    object List {
      object Case {
        def apply(caseValue: ValueCase.ListCase[Value[Any]]): List[Any] =
          List(caseValue.elements, ZEnvironment.empty)
        def apply[Annotations](
            caseValue: ValueCase.ListCase[Value[Annotations]],
            annotations: ZEnvironment[Annotations]
        ): List[Annotations] =
          List(caseValue.elements, annotations)
      }
    }

    final case class LetDefinition[Annotations] private[morphir] (
        name: Name,
        definition: Definition[Annotations],
        inValue: Value[Annotations],
        annotations: ZEnvironment[Annotations]
    ) extends Value[Annotations] {
      override val caseValue: ValueCase[Value[Annotations]] = LetDefinitionCase(name, definition, inValue)
    }

    object LetDefinition {
      object Case {
        // def apply(caseValue: ValueCase.LetDefinitionCase[Value[Any]]): LetDefinition[Any] =
        //   LetDefinition(caseValue.valueName, caseValue.valueDefinition, caseValue.inValue, ZEnvironment.empty)
        // def apply[Annotations](
        //     caseValue: ValueCase.LetDefinitionCase[Value[Annotations]],
        //     annotations: ZEnvironment[Annotations]
        // ): LetDefinition[Annotations] =
        //   LetDefinition(caseValue.valueName, caseValue.valueDefinition, caseValue.inValue, annotations)
      }
    }

    final case class LetRecursion[Annotations] private[morphir] (
        dict: Map[Name, Definition[Annotations]],
        inValue: Value[Annotations],
        annotations: ZEnvironment[Annotations]
    ) extends Value[Annotations] {
      override val caseValue: ValueCase[Value[Annotations]] = LetRecursionCase(dict, inValue)
    }

    object LetRecursion {
      object Case {
        // def apply(caseValue: ValueCase.LetRecursionCase[Value[Any]]): LetRecursion[Any] =
        //   LetRecursion(caseValue.valueDefinitions, caseValue.inValue, ZEnvironment.empty)
        // def apply[Annotations](
        //     caseValue: ValueCase.LetRecursionCase[Value[Annotations]],
        //     annotations: ZEnvironment[Annotations]
        // ): LetRecursion[Annotations] =
        //   LetRecursion(caseValue.valueDefinitions, caseValue.inValue, annotations)
      }
    }

    // todo fix Definition
    final case class Definition[Annotations] private[morphir] (annotations: ZEnvironment[Annotations])
        extends Value[Annotations] {
      override def caseValue: UnitCase = ValueCase.UnitCase
    }

    final case class Destructure[Annotations] private[morphir] (
        pattern: Pattern[Annotations],
        valueToDestruct: Value[Annotations],
        inValue: Value[Annotations],
        annotations: ZEnvironment[Annotations]
    ) extends Value[Annotations] {
      override val caseValue: ValueCase[Value[Annotations]] = DestructureCase(pattern, valueToDestruct, inValue)
    }

    object Destructure {
      object Case {
        // def apply(caseValue: ValueCase.DestructureCase[Value[Any]]): Destructure[Any] =
        //   Destructure(caseValue.pattern, caseValue.valueToDestruct, caseValue.inValue, ZEnvironment.empty)
        // def apply[Annotations](
        //     caseValue: ValueCase.DestructureCase[Value[Annotations]],
        //     annotations: ZEnvironment[Annotations]
        // ): Destructure[Annotations] =
        //   Destructure(caseValue.pattern, caseValue.valueToDestruct, caseValue.inValue, annotations)
      }
    }

    final case class IfThenElse[Annotations] private[morphir] (
        condition: Value[Annotations],
        thenBranch: Value[Annotations],
        elseBranch: Value[Annotations],
        annotations: ZEnvironment[Annotations]
    ) extends Value[Annotations] {
      override val caseValue: ValueCase[Value[Annotations]] = IfThenElseCase(condition, thenBranch, elseBranch)
    }

    object IfThenElse {
      object Case {
        def apply(caseValue: ValueCase.IfThenElseCase[Value[Any]]): IfThenElse[Any] =
          IfThenElse(caseValue.condition, caseValue.thenBranch, caseValue.elseBranch, ZEnvironment.empty)
        def apply[Annotations](
            caseValue: ValueCase.IfThenElseCase[Value[Annotations]],
            annotations: ZEnvironment[Annotations]
        ): IfThenElse[Annotations] =
          IfThenElse(caseValue.condition, caseValue.thenBranch, caseValue.elseBranch, annotations)
      }
    }

    final case class UpdateRecord[Annotations] private[morphir] (
        valueToUpdate: Value[Annotations],
        fieldsToUpdate: Chunk[(Name, Value[Annotations])],
        annotations: ZEnvironment[Annotations]
    ) extends Value[Annotations] {
      override val caseValue: ValueCase[Value[Annotations]] = UpdateRecordCase(valueToUpdate, fieldsToUpdate)
    }

    object UpdateRecord {
      object Case {
        def apply(caseValue: ValueCase.UpdateRecordCase[Value[Any]]): UpdateRecord[Any] =
          UpdateRecord(caseValue.valueToUpdate, caseValue.fieldsToUpdate, ZEnvironment.empty)
        def apply[Annotations](
            caseValue: ValueCase.UpdateRecordCase[Value[Annotations]],
            annotations: ZEnvironment[Annotations]
        ): UpdateRecord[Annotations] =
          UpdateRecord(caseValue.valueToUpdate, caseValue.fieldsToUpdate, annotations)
      }
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

  // class Dog(name) = {
  //  name : String = "Spot"
  // }

  // GenericRecord("Dog", Map("name" -> "Spot"))
  // val myDog = if true then Dog("Spot") else Dog("Fuzzy")
  // myDog match
  // case Dog(name) => name

  // ApplyCase(ConstructorCase("Person"), Chunk(FieldCase("Name"), LiteralCase("Adam")), FieldCase("Person", LiteralCase("42"))))
  // ApplyCase(function, args)

  // let myFunction = actualFunction ..
  // ApplyCase(myFunction, args) // actually uses "myFunction"

  // Person("Adam", 42)

  // case ApplyCase(fun, args) =
  // val theFunction = interpret(fun)
  // apply(theFunction, args.map(interpret))

  object ValueCase {
    final case class NativeApplyCase[+Self](function: NativeFunction, arguments: Chunk[Self]) extends ValueCase[Self]
    final case class ApplyCase[+Self](function: Self, arguments: Chunk[Self])                 extends ValueCase[Self]
    final case class ConstructorCase(name: FQName)                                            extends ValueCase[Nothing]
    final case class FieldCase[+Self](target: Self, name: Name)                               extends ValueCase[Self]
    final case class FieldFunctionCase(name: Name)                                            extends ValueCase[Nothing]
    final case class IfThenElseCase[+Self](condition: Self, thenBranch: Self, elseBranch: Self) extends ValueCase[Self]
    final case class ListCase[+Self](elements: Chunk[Self])                                     extends ValueCase[Self]
    final case class LiteralCase[+A](literal: Lit[A])                                       extends ValueCase[Nothing]
    final case class PatternMatchCase[+Self](branchOutOn: Self, cases: Chunk[(Self, Self)]) extends ValueCase[Self]
    final case class RecordCase[+Self](fields: Chunk[(Name, Self)])                         extends ValueCase[Self]
    final case class ReferenceCase(name: FQName)                                            extends ValueCase[Nothing]
    final case class TupleCase[+Self](elements: Chunk[Self])                                extends ValueCase[Self]
    case object UnitCase                                                                    extends ValueCase[Nothing]
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
    object PatternCase {

      final case class AsPatternCase[+Self](pattern: Self, name: Name) extends PatternCase[Self]
      final case class ConstructorPatternCase[+Self](constructorName: FQName, argumentPatterns: Chunk[Self])
          extends PatternCase[Self]
      type EmptyListPatternCase = EmptyListPatternCase.type
      case object EmptyListPatternCase                                    extends PatternCase[Nothing]
      final case class HeadTailPatternCase[+Self](head: Self, tail: Self) extends PatternCase[Self]
      final case class LiteralPatternCase(value: LiteralValue)            extends PatternCase[Nothing]
      final case class TuplePatternCase[+Self](elements: Chunk[Self])     extends PatternCase[Self]
      type UnitPatternCase = UnitPatternCase.type
      case object UnitPatternCase extends PatternCase[Nothing]
      type WildcardPatternCase = WildcardPatternCase.type
      case object WildcardPatternCase extends PatternCase[Nothing]

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
