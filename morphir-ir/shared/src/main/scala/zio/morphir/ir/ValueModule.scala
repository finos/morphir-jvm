package zio.morphir.ir

import zio.morphir.ir.{Literal => Lit}
import zio.morphir.ir.TypeModule.Type
import zio.{Chunk, ZEnvironment, ZIO}
import zio.prelude.*
import zio.prelude.fx.ZPure
import zio.morphir.syntax.ValueSyntax

object ValueModule {

  def mapDefinition[Annotations, Err](definition: ValueDefinition[Annotations])(
      tryMapType: Type[Annotations] => Validation[Err, Type[Annotations]],
      tryMapValue: Value[Annotations] => Validation[Err, Value[Annotations]]
  ): Validation[Err, ValueDefinition[Annotations]] = ???

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

  // type alias Definition ta va =
  //   { inputTypes : List ( Name, va, Type ta )
  //   , outputType : Type ta
  //   , body : Value ta va
  //   }

  final case class TypeAscription(tpe: UType)

  type ValueDefinition[+Annotations] = Definition[Value[Annotations], Annotations]
  val ValueDefinition = Definition

  final case class Definition[+Self, +Annotations](
      inputTypes: Chunk[InputParameter[Annotations]],
      outputType: Type[Annotations],
      body: Self
  ) { self =>
    final def toSpecification: Specification[Annotations] =
      Specification(inputTypes.map(input => (input.name, input.tpe)), outputType)

    final def toValue[Annotations1 >: Annotations](implicit ev: Self <:< Value[Annotations1]): Value[Annotations1] = {
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
    ): Validation[Err, Definition[Self, Annotations2]] = {
      ???
    }
    // f(outputType).map(outputType => self.copy(outputType = outputType))

    def map[Self2](f: Self => Self2): Definition[Self2, Annotations] =
      self.copy(body = f(self.body))

    // def fold

    def forEach[G[+_]: IdentityBoth: Covariant, Self2](f: Self => G[Self2]): G[Definition[Self2, Annotations]] =
      f(self.body).map(body => self.copy(body = body))
  }

  object Definition {
    def fromLiteral[A](value: Value[Any]): Definition[Value[Any], Any] =
      Definition(Chunk.empty, TypeModule.Type.unit, value)

    def fromTypedValue(value: TypedValue): ValueDefinition[Any] = {
      val typeAttrib = value.annotations.get[UType]
      Definition(Chunk.empty, typeAttrib, value)
    }

    def fromTypedValue(value: Value[Any], valueType: UType): ValueDefinition[Any] = {
      Definition(Chunk.empty, valueType, value)
    }
  }
  final case class InputParameter[+Annotations](
      name: Name,
      tpe: Type[Annotations],
      annotations: ZEnvironment[Annotations]
  ) {
    def toValue[A >: Annotations](body: Value[A]): Value[A] =
      Value(
        ValueCase
          .LambdaCase[A, Value[A]](
            Pattern.AsPattern[A](Pattern.wildcardPattern[A](annotations), name, annotations),
            body
          ),
        annotations
      )
  }

  final type RawValue = Value[Any]
  final val RawValue = Value

  final case class Specification[+Annotations](inputs: Chunk[(Name, Type[Annotations])], output: Type[Annotations]) {
    def mapSpecificationAttributes[B](func: Annotations => B): Specification[B] = ???
  }

  object Specification {
    def create[Annotations](inputs: (Name, Type[Annotations])*): Inputs[Annotations] =
      new Inputs(() => Chunk.fromIterable(inputs))

    final class Inputs[Annotations](private val inputs: () => Chunk[(Name, Type[Annotations])]) extends AnyVal {
      def apply(output: Type[Annotations]): Specification[Annotations] =
        Specification(inputs(), output)
    }
  }

  final type TypedValue = Value[UType]
  val TypedValue = Value

  final case class Value[+Annotations] private[morphir] (
      caseValue: ValueCase[Value[Annotations]],
      annotations: ZEnvironment[Annotations]
  ) { self =>
    import ValueCase.*
    import Value.*

    def fold[Z](f: ValueCase[Z] => Z): Z = self.caseValue match {
      case c @ ValueCase.ApplyCase(_, _)    => f(ValueCase.ApplyCase(c.function.fold(f), c.arguments.map(_.fold(f))))
      case c @ ValueCase.ConstructorCase(_) => f(ValueCase.ConstructorCase(c.name))
      case c @ ValueCase.DestructureCase(_, _, _) =>
        f(ValueCase.DestructureCase(c.pattern, c.valueToDestruct.fold(f), c.inValue.fold(f)))
      case c @ ValueCase.FieldCase(_, _)      => f(ValueCase.FieldCase(c.target.fold(f), c.name))
      case c @ ValueCase.FieldFunctionCase(_) => f(c)
      case c @ ValueCase.IfThenElseCase(_, _, _) =>
        f(ValueCase.IfThenElseCase(c.condition.fold(f), c.thenBranch.fold(f), c.elseBranch.fold(f)))
      case c @ ValueCase.LambdaCase(_, _) => f(ValueCase.LambdaCase(c.argumentPattern, c.body.fold(f)))
      case c @ ValueCase.LetDefinitionCase(_, _, _) =>
        f(ValueCase.LetDefinitionCase(c.valueName, c.valueDefinition.map(_.fold(f)), c.inValue.fold(f)))
      case c @ ValueCase.LetRecursionCase(_, _) =>
        f(
          ValueCase.LetRecursionCase(
            c.valueDefinitions.map { case (name, value) => (name, value.map(_.fold(f))) },
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
              (pattern, value.fold(f))
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
      case c @ ValueCase.ApplyCase(_, _)    => apply(c.function, c.arguments)
      case c @ ValueCase.ConstructorCase(_) => constructor(c.name)
      case c @ ValueCase.DestructureCase(_, _, _) =>
        destructure(c.pattern, c.valueToDestruct, c.inValue)
      case c @ ValueCase.FieldCase(_, _)      => field(c.target, c.name)
      case c @ ValueCase.FieldFunctionCase(_) => fieldFunction(c.name)
      case c @ ValueCase.IfThenElseCase(_, _, _) =>
        ifThenElse(c.condition, c.thenBranch, c.elseBranch)
      case c @ ValueCase.LambdaCase(_, _) => lambda(c.argumentPattern, c.body)
      case c @ ValueCase.LetDefinitionCase(_, _, _) =>
        letDefinition(c.valueName, c.valueDefinition, c.inValue)
      case c @ ValueCase.LetRecursionCase(_, _) =>
        letRecursion(c.valueDefinitions, c.inValue)
      case c @ ValueCase.ListCase(_)            => list(c.elements)
      case c @ ValueCase.LiteralCase(_)         => literal(c.literal)
      case c @ ValueCase.NativeApplyCase(_, _)  => nativeApply(c.function, c.arguments)
      case c @ ValueCase.PatternMatchCase(_, _) => patternMatch(c.branchOutOn, c.cases)
      case c @ ValueCase.ReferenceCase(_)       => reference(c.name)
      case c @ ValueCase.RecordCase(_)          => record(c.fields)
      case c @ ValueCase.TupleCase(_)           => tuple(c.elements)
      case _ @ValueCase.UnitCase                => unit
      case c @ ValueCase.UpdateRecordCase(_, _) =>
        updateRecord(c.valueToUpdate, c.fieldsToUpdate)
      case c @ ValueCase.VariableCase(_) => variable(c.name)
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
      case c @ ValueCase.ApplyCase(_, _)            => c.function ++ c.arguments.flatten
      case c @ ValueCase.DestructureCase(_, _, _)   => c.valueToDestruct ++ c.inValue
      case c @ ValueCase.FieldCase(_, _)            => c.target
      case c @ ValueCase.FieldFunctionCase(_)       => Set(c.name)
      case c @ ValueCase.IfThenElseCase(_, _, _)    => c.condition ++ c.thenBranch ++ c.elseBranch
      case c @ ValueCase.LambdaCase(_, _)           => c.body
      case c @ ValueCase.LetDefinitionCase(_, _, _) => c.valueDefinition.body ++ c.inValue + c.valueName
      case c @ ValueCase.LetRecursionCase(_, _) =>
        c.valueDefinitions.foldLeft(Set.empty[Name])((acc, kv) => acc ++ kv._2.body + kv._1)
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
      case c @ ValueCase.ApplyCase(_, _)            => c.function ++ c.arguments.flatten
      case c @ ValueCase.DestructureCase(_, _, _)   => c.valueToDestruct ++ c.inValue
      case c @ ValueCase.FieldCase(_, _)            => c.target
      case _ @ValueCase.FieldFunctionCase(_)        => Set.empty
      case c @ ValueCase.IfThenElseCase(_, _, _)    => c.condition ++ c.thenBranch ++ c.elseBranch
      case c @ ValueCase.LambdaCase(_, _)           => c.body
      case c @ ValueCase.LetDefinitionCase(_, _, _) => c.valueDefinition.body ++ c.inValue
      case c @ ValueCase.LetRecursionCase(_, _) =>
        c.valueDefinitions.foldLeft(Set.empty[FQName])((acc, kv) => acc ++ kv._2.body)
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

    def toDefinition: ValueDefinition[Annotations] = {
      // HACK: This is not correct it needs to be made correct
      ValueDefinition(Chunk.empty, Type.unit[Annotations](annotations), self)
    }
  }

  object Value extends ValueSyntax {
    def apply(caseValue: ValueCase[Value[Any]]): Value[Any] = Value(caseValue, ZEnvironment.empty)

  }

  sealed trait ValueCase[+Self] { self =>
    import ValueCase.*

    def map[B](f: Self => B): ValueCase[B] = self match {
      case c @ ApplyCase(_, _)          => ApplyCase(f(c.function), c.arguments.map(f))
      case c @ ConstructorCase(_)       => ConstructorCase(c.name)
      case c @ DestructureCase(_, _, _) => DestructureCase(c.pattern, f(c.valueToDestruct), f(c.inValue))
      case c @ FieldCase(_, _)          => FieldCase(f(c.target), c.name)
      case c @ FieldFunctionCase(_)     => FieldFunctionCase(c.name)
      case c @ IfThenElseCase(_, _, _) =>
        IfThenElseCase(f(c.condition), f(c.thenBranch), f(c.elseBranch))
      case c @ LambdaCase(_, _)           => LambdaCase(c.argumentPattern, f(c.body))
      case c @ LetDefinitionCase(_, _, _) => LetDefinitionCase(c.valueName, c.valueDefinition.map(f), f(c.inValue))
      case c @ LetRecursionCase(_, _) =>
        LetRecursionCase(c.valueDefinitions.map { case (name, value) => (name, value.map(f)) }, f(c.inValue))
      case c @ ListCase(_)           => ListCase(c.elements.map(f))
      case c @ LiteralCase(_)        => LiteralCase(c.literal)
      case c @ NativeApplyCase(_, _) => NativeApplyCase(c.function, c.arguments.map(f))
      case c @ PatternMatchCase(_, _) =>
        PatternMatchCase(f(c.branchOutOn), c.cases.map { case (p, v) => (p, f(v)) })
      case c @ RecordCase(_)    => RecordCase(c.fields.map { case (name, value) => (name, f(value)) })
      case c @ ReferenceCase(_) => c
      case c @ TupleCase(_)     => TupleCase(c.elements.map(f))
      case _ @UnitCase          => UnitCase
      case c @ UpdateRecordCase(_, _) =>
        UpdateRecordCase(f(c.valueToUpdate), c.fieldsToUpdate.map { case (name, self) => (name, f(self)) })
      case c @ VariableCase(_) => c

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
    final case class LiteralCase[+A](literal: Lit[A]) extends ValueCase[Nothing]
    final case class PatternMatchCase[+Annotations, +Self](
        branchOutOn: Self,
        cases: Chunk[(Pattern[Annotations], Self)]
    ) extends ValueCase[Self]
    final case class RecordCase[+Self](fields: Chunk[(Name, Self)]) extends ValueCase[Self]
    final case class ReferenceCase(name: FQName)                    extends ValueCase[Nothing]
    final case class TupleCase[+Self](elements: Chunk[Self])        extends ValueCase[Self]
    case object UnitCase                                            extends ValueCase[Nothing]
    type UnitCase = UnitCase.type
    final case class VariableCase(name: Name) extends ValueCase[Nothing]
    final case class LetDefinitionCase[+Annotations, +Self](
        valueName: Name,
        valueDefinition: Definition[Self, Annotations],
        inValue: Self
    ) extends ValueCase[Self]
    final case class LetRecursionCase[+Annotations, +Self](
        valueDefinitions: Map[Name, Definition[Self, Annotations]],
        inValue: Self
    ) extends ValueCase[Self]
    final case class UpdateRecordCase[+Self](valueToUpdate: Self, fieldsToUpdate: Chunk[(Name, Self)])
        extends ValueCase[Self]
    final case class LambdaCase[+Annotations, +Self](argumentPattern: Pattern[Annotations], body: Self)
        extends ValueCase[Self]
    final case class DestructureCase[+Annotations, +Self](
        pattern: Pattern[Annotations],
        valueToDestruct: Self,
        inValue: Self
    ) extends ValueCase[Self]
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
              (f(c.valueToDestruct), f(c.inValue)).mapN(DestructureCase(c.pattern, _, _))
            case c @ FieldCase(_, _)      => f(c.target).map(FieldCase(_, c.name))
            case c @ FieldFunctionCase(_) => c.succeed
            case c @ IfThenElseCase(_, _, _) =>
              (f(c.condition), f(c.thenBranch), f(c.elseBranch)).mapN(IfThenElseCase(_, _, _))
            case c @ LambdaCase(_, _) => f(c.body).map(LambdaCase(c.argumentPattern, _))
            case c @ LetDefinitionCase(_, _, _) =>
              c.valueDefinition.forEach(f).zipWith(f(c.inValue))(LetDefinitionCase(c.valueName, _, _))
            case c @ LetRecursionCase(_, _) =>
              c.valueDefinitions.forEach(_.forEach(f)).zipWith(f(c.inValue))(LetRecursionCase(_, _))
            case c @ ListCase(_)           => c.elements.forEach(f).map(ListCase(_))
            case c @ LiteralCase(_)        => c.succeed
            case c @ NativeApplyCase(_, _) => c.arguments.forEach(f).map(NativeApplyCase(c.function, _))
            case c @ PatternMatchCase(_, _) =>
              f(c.branchOutOn)
                .zipWith(c.cases.forEach { case (key, value) => f(value).map(value => key -> value) })(
                  PatternMatchCase(_, _)
                )
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
          }
      }
  }

}
