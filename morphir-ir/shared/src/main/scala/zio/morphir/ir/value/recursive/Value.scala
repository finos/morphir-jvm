package zio.morphir.ir.value.recursive

import zio.morphir.ir.Type.{Type, UType}
import zio.morphir.ir.value.{Pattern, PatternConstructors, UPattern}
import zio.morphir.ir.{FQName, Literal => Lit, Name, Path}
import zio.prelude._
import zio.prelude.fx.ZPure
import zio.{Chunk, NonEmptyChunk, ZIO}

import scala.annotation.tailrec

final case class Value[+TA, +VA](caseValue: ValueCase[TA, VA, Value[TA, VA]]) { self =>
  import ValueCase._
  def attributes: VA = caseValue.attributes

  def collectReferences: Set[FQName] = fold[Set[FQName]] {
    case c @ ApplyCase(_, _, _)            => c.function ++ c.argument
    case c @ DestructureCase(_, _, _, _)   => c.valueToDestruct ++ c.inValue
    case c @ FieldCase(_, _, _)            => c.target
    case _ @FieldFunctionCase(_, _)        => Set.empty
    case c @ IfThenElseCase(_, _, _, _)    => c.condition ++ c.thenBranch ++ c.elseBranch
    case c @ LambdaCase(_, _, _)           => c.body
    case c @ LetDefinitionCase(_, _, _, _) => c.valueDefinition.body ++ c.inValue
    case c @ LetRecursionCase(_, _, _) =>
      c.valueDefinitions.foldLeft(Set.empty[FQName])((acc, kv) => acc ++ kv._2.body)
    case c @ ListCase(_, _)            => c.elements.flatten.toSet
    case _ @LiteralCase(_, _)          => Set.empty
    case c @ PatternMatchCase(_, _, _) => c.cases.flatMap(_._2).toSet ++ c.branchOutOn
    case c @ RecordCase(_, _)          => c.fields.flatMap(_._2).toSet
    case c @ ReferenceCase(_, _)       => Set(c.name)
    case c @ TupleCase(_, _)           => c.elements.flatten.toSet
    case _ @UnitCase(_)                => Set.empty
    case c @ UpdateRecordCase(_, _, _) => c.fieldsToUpdate.flatMap(_._2).toSet ++ c.valueToUpdate
    case _ @VariableCase(_, _)         => Set.empty
    case _                             => Set.empty
  }
  def collectVariables: Set[Name] = fold[Set[Name]] {
    case c @ ApplyCase(_, _, _)            => c.function ++ c.argument
    case c @ DestructureCase(_, _, _, _)   => c.valueToDestruct ++ c.inValue
    case c @ FieldCase(_, _, _)            => c.target
    case c @ IfThenElseCase(_, _, _, _)    => c.condition ++ c.thenBranch ++ c.elseBranch
    case c @ LambdaCase(_, _, _)           => c.body
    case c @ LetDefinitionCase(_, _, _, _) => c.valueDefinition.body ++ c.inValue + c.valueName
    case c @ LetRecursionCase(_, _, _) =>
      c.valueDefinitions.foldLeft(Set.empty[Name])((acc, kv) => acc ++ kv._2.body + kv._1)
    case c @ ListCase(_, _)            => c.elements.flatten.toSet
    case _ @LiteralCase(_, _)          => Set.empty
    case c @ PatternMatchCase(_, _, _) => c.cases.flatMap(_._2).toSet ++ c.branchOutOn
    case c @ RecordCase(_, _)          => c.fields.flatMap(_._2).toSet
    case c @ TupleCase(_, _)           => c.elements.flatten.toSet
    case _ @UnitCase(_)                => Set.empty
    case c @ UpdateRecordCase(_, _, _) => c.fieldsToUpdate.flatMap(_._2).toSet ++ c.valueToUpdate
    case c @ VariableCase(_, _)        => Set(c.name)
    case _                             => Set.empty
  }

  def fold[Z](f: (ValueCase[TA, VA, Z]) => Z): Z = self.caseValue match {
    case c @ ApplyCase(_, _, _)    => f(ApplyCase(c.attributes, c.function.fold(f), c.argument.fold(f)))
    case c @ ConstructorCase(_, _) => f(c)
    case c @ DestructureCase(_, _, _, _) =>
      f(DestructureCase(c.attributes, c.pattern, c.valueToDestruct.fold(f), c.inValue.fold(f)))
    case c @ FieldCase(_, _, _)      => f(FieldCase(c.attributes, c.target.fold(f), c.name))
    case c @ FieldFunctionCase(_, _) => f(c)
    case c @ IfThenElseCase(_, _, _, _) =>
      f(IfThenElseCase(c.attributes, c.condition.fold(f), c.thenBranch.fold(f), c.elseBranch.fold(f)))
    case c @ LambdaCase(_, _, _) => f(LambdaCase(c.attributes, c.argumentPattern, c.body.fold(f)))
    case c @ LetDefinitionCase(_, _, _, _) =>
      f(LetDefinitionCase(c.attributes, c.valueName, c.valueDefinition.map(_.fold(f)), c.inValue.fold(f)))
    case c @ LetRecursionCase(_, _, _) =>
      f(
        LetRecursionCase(
          c.attributes,
          c.valueDefinitions.map { case (n, v) => (n, v.map(_.fold(f))) },
          c.inValue.fold(f)
        )
      )
    case c @ ListCase(_, _)    => f(ListCase(c.attributes, c.elements.map(_.fold(f))))
    case c @ LiteralCase(_, _) => f(c)
    case c @ PatternMatchCase(_, _, _) =>
      f(PatternMatchCase(c.attributes, c.branchOutOn.fold(f), c.cases.map { case (p, v) => (p, v.fold(f)) }))
    case c @ RecordCase(_, _)    => f(RecordCase(c.attributes, c.fields.map { case (n, v) => (n, v.fold(f)) }))
    case c @ ReferenceCase(_, _) => f(c)
    case c @ TupleCase(_, _)     => f(TupleCase(c.attributes, c.elements.map(_.fold(f))))
    case c @ UnitCase(_)         => f(c)
    case c @ UpdateRecordCase(_, _, _) =>
      f(UpdateRecordCase(c.attributes, c.valueToUpdate.fold(f), c.fieldsToUpdate.map { case (n, v) => (n, v.fold(f)) }))
    case c @ VariableCase(_, _) => f(c)
  }

  def foldDown[Z](z: Z)(f: (Z, Value[TA, VA]) => Z): Z =
    caseValue.foldLeft(f(z, self))((z, recursive) => recursive.foldDown(z)(f))

  def foldDownSome[Z](z: Z)(pf: PartialFunction[(Z, Value[TA, VA]), Z]): Z =
    foldDown(z)((z, recursive) => pf.lift(z -> recursive).getOrElse(z))

  def foldLeft[Z](initial: Z)(f: (Z, Value[TA, VA]) => Z): Z = {
    @tailrec
    def loop(stack: List[Value[TA, VA]], acc: Z): Z =
      stack match {
        case Nil                                      => acc
        case Value(v @ ApplyCase(_, _, _)) :: tail    => loop(v.function :: v.argument :: tail, f(acc, Value(v)))
        case Value(v @ ConstructorCase(_, _)) :: tail => loop(tail, f(acc, Value(v)))
        case Value(v @ DestructureCase(_, _, _, _)) :: tail =>
          loop(v.valueToDestruct :: v.inValue :: tail, f(acc, Value(v)))
        case Value(v @ FieldCase(_, _, _)) :: tail      => loop(v.target :: tail, f(acc, Value(v)))
        case Value(v @ FieldFunctionCase(_, _)) :: tail => loop(tail, f(acc, Value(v)))
        case Value(v @ IfThenElseCase(_, _, _, _)) :: tail =>
          loop(v.condition :: v.thenBranch :: v.elseBranch :: tail, f(acc, Value(v)))
        case Value(v @ LambdaCase(_, _, _)) :: tail => loop(v.body :: tail, f(acc, Value(v)))
        case Value(v @ LetDefinitionCase(_, _, _, _)) :: tail =>
          loop(v.valueDefinition.body :: v.inValue :: tail, f(acc, Value(v)))
        case Value(v @ LetRecursionCase(_, _, _)) :: tail =>
          loop(v.valueDefinitions.map(_._2.body).toList ::: v.inValue :: tail, f(acc, Value(v)))
        case Value(v @ ListCase(_, _)) :: tail    => loop(v.elements.toList ::: tail, f(acc, Value(v)))
        case Value(v @ LiteralCase(_, _)) :: tail => loop(tail, f(acc, Value(v)))
        case Value(v @ PatternMatchCase(_, _, _)) :: tail =>
          loop(v.branchOutOn :: v.cases.map(_._2).toList ::: tail, f(acc, Value(v)))
        case Value(v @ RecordCase(_, _)) :: tail    => loop(v.fields.map(_._2).toList ::: tail, f(acc, Value(v)))
        case Value(v @ ReferenceCase(_, _)) :: tail => loop(tail, f(acc, Value(v)))
        case Value(v @ TupleCase(_, _)) :: tail     => loop(v.elements.toList ::: tail, f(acc, Value(v)))
        case Value(v @ UnitCase(_)) :: tail         => loop(tail, f(acc, Value(v)))
        case Value(v @ UpdateRecordCase(_, _, _)) :: tail =>
          loop(v.valueToUpdate :: v.fieldsToUpdate.map(_._2).toList ::: tail, f(acc, Value(v)))
        case Value(v @ VariableCase(_, _)) :: tail => loop(tail, f(acc, Value(v)))
      }

    loop(List(self), initial)
  }

  def foldM[F[+_]: AssociativeFlatten: Covariant: IdentityBoth, Z](f: ValueCase[TA, VA, Z] => F[Z]): F[Z] =
    fold[F[Z]](_.flip.flatMap(f))

  def foldPure[W, S, R, E, Z](f: ValueCase[TA, VA, Z] => ZPure[W, S, S, R, E, Z]): ZPure[W, S, S, R, E, Z] =
    foldM(f)

  def foldRecursive[Z](f: ValueCase[TA, VA, (Value[TA, VA], Z)] => Z): Z =
    f(caseValue.map(recursive => recursive -> recursive.foldRecursive(f)))

  def foldUp[Z](z: Z)(f: (Z, Value[TA, VA]) => Z): Z =
    f(caseValue.foldLeft(z)((z, recursive) => recursive.foldUp(z)(f)), self)

  def foldZIO[R, E, Z](f: ValueCase[TA, VA, Z] => ZIO[R, E, Z]): ZIO[R, E, Z] = foldM(f)

  def isData: Boolean = foldLeft[Boolean](true) {
    case (acc, Value(LiteralCase(_, _)))                => acc && true
    case (acc, Value(ConstructorCase(_, _)))            => acc && true
    case (acc, Value(TupleCase(_, elements)))           => acc && elements.forall(_.isData)
    case (acc, Value(ListCase(_, elements)))            => acc && elements.forall(_.isData)
    case (acc, Value(RecordCase(_, fields)))            => acc && fields.forall(_._2.isData)
    case (acc, Value(ApplyCase(_, function, argument))) =>
      // most Apply nodes will be logic but if it's a Constructor with arguments it is still considered data
      acc && function.isData && argument.isData
    case (acc, Value(UnitCase(_))) => acc && true
    case _                         =>
      // everything else is considered logic
      false
  }

  def mapAttributes[TB, VB](f: TA => TB, g: VA => VB): Value[TB, VB] = fold[Value[TB, VB]] {
    case ApplyCase(attributes, function, argument) => Value(ApplyCase(g(attributes), function, argument))
    case ConstructorCase(attributes, name)         => Value(ConstructorCase(g(attributes), name))
    case DestructureCase(attributes, pattern, valueToDestruct, inValue) =>
      Value(DestructureCase(g(attributes), pattern.map(g), valueToDestruct, inValue))
    case FieldCase(attributes, target, name) => Value(FieldCase(g(attributes), target, name))
    case FieldFunctionCase(attributes, name) => Value(FieldFunctionCase(g(attributes), name))
    case IfThenElseCase(attributes, condition, thenBranch, elseBranch) =>
      Value(IfThenElseCase(g(attributes), condition, thenBranch, elseBranch))
    case LambdaCase(attributes, argumentPattern, body) => Value(LambdaCase(g(attributes), argumentPattern.map(g), body))
    case LetDefinitionCase(attributes, valueName, valueDefinition, inValue) =>
      Value(LetDefinitionCase(g(attributes), valueName, valueDefinition.mapAttributes(f, g), inValue))
    case LetRecursionCase(attributes, valueDefinitions, inValue) =>
      Value(
        LetRecursionCase(g(attributes), valueDefinitions.map { case (n, v) => (n, v.mapAttributes(f, g)) }, inValue)
      )
    case ListCase(attributes, elements)   => Value(ListCase(g(attributes), elements))
    case LiteralCase(attributes, literal) => Value(LiteralCase(g(attributes), literal))
    case PatternMatchCase(attributes, branchOutOn, cases) =>
      Value(PatternMatchCase(g(attributes), branchOutOn, cases.map { case (p, v) => (p.map(g), v) }))
    case RecordCase(attributes, fields)  => Value(RecordCase(g(attributes), fields))
    case ReferenceCase(attributes, name) => Value(ReferenceCase(g(attributes), name))
    case TupleCase(attributes, elements) => Value(TupleCase(g(attributes), elements))
    case UnitCase(attributes)            => Value(UnitCase(g(attributes)))
    case UpdateRecordCase(attributes, valueToUpdate, fieldsToUpdate) =>
      Value(UpdateRecordCase(g(attributes), valueToUpdate, fieldsToUpdate))
    case VariableCase(attributes, name) => Value(VariableCase(g(attributes), name))
  }

  // def mapTypeAttributes[TB](f: TA => TB): ValueCase[TB, VA, Self] = self.caseValue match {
  //   case ApplyCase(attributes, function, argument)                          => ???
  //   case ConstructorCase(attributes, name)                                  => ???
  //   case DestructureCase(attributes, pattern, valueToDestruct, inValue)     => ???
  //   case FieldCase(attributes, target, name)                                => ???
  //   case FieldFunctionCase(attributes, name)                                => ???
  //   case IfThenElseCase(attributes, condition, thenBranch, elseBranch)      => ???
  //   case LambdaCase(attributes, argumentPattern, body)                      => ???
  //   case LetDefinitionCase(attributes, valueName, valueDefinition, inValue) => ???
  //   case LetRecursionCase(attributes, valueDefinitions, inValue)            => ???
  //   case ListCase(attributes, elements)                                     => ???
  //   case LiteralCase(attributes, literal)                                   => ???
  //   case PatternMatchCase(attributes, branchOutOn, cases)                   => ???
  //   case RecordCase(attributes, fields)                                     => ???
  //   case ReferenceCase(attributes, name)                                    => ???
  //   case TupleCase(attributes, elements)                                    => ???
  //   case UnitCase(attributes)   => ???
  //   case UpdateRecordCase(attributes, valueToUpdate, fieldsToUpdate)        => ???
  //   case VariableCase(attributes, name)                                     => ???
  // }

  // def mapValueAttributes[VB](f: VA => VB): ValueCase[TA, VB, Self] = self match {
  //   case ApplyCase(attributes, function, argument)                          => ???
  //   case ConstructorCase(attributes, name)                                  => ???
  //   case DestructureCase(attributes, pattern, valueToDestruct, inValue)     => ???
  //   case FieldCase(attributes, target, name)                                => ???
  //   case FieldFunctionCase(attributes, name)                                => ???
  //   case IfThenElseCase(attributes, condition, thenBranch, elseBranch)      => ???
  //   case LambdaCase(attributes, argumentPattern, body)                      => ???
  //   case LetDefinitionCase(attributes, valueName, valueDefinition, inValue) => ???
  //   case LetRecursionCase(attributes, valueDefinitions, inValue)            => ???
  //   case ListCase(attributes, elements)                                     => ???
  //   case LiteralCase(attributes, literal)                                   => ???
  //   case PatternMatchCase(attributes, branchOutOn, cases)                   => ???
  //   case RecordCase(attributes, fields)                                     => ???
  //   case ReferenceCase(attributes, name)                                    => ???
  //   case TupleCase(attributes, elements)                                    => ???
  //   case UnitCase(attributes)                                               => ???
  //   case UpdateRecordCase(attributes, valueToUpdate, fieldsToUpdate)        => ???
  //   case VariableCase(attributes, name)                                     => ???
  // }

  def toRawValue: RawValue = mapAttributes(_ => (), _ => ())

  override def toString: String = foldRecursive[String] {
    case ApplyCase(attributes, (_, function), (_, argument)) => s"$function $argument"
    case ConstructorCase(_, name)                            => name.toReferenceName
    case DestructureCase(_, pattern, (_, valueToDestruct), (_, inValue)) =>
      s"let $pattern = $valueToDestruct in $inValue"
    case FieldCase(_, (_, target), name) => s"$target.${name.toCamelCase}"
    case FieldFunctionCase(_, name)      => s".${name.toCamelCase}"
    case IfThenElseCase(_, (_, condition), (_, thenBranch), (_, elseBranch)) =>
      s"if $condition then $thenBranch else $elseBranch"
    case LambdaCase(_, argumentPattern, (_, body)) => s"(\\$argumentPattern -> $body)"
    case LetDefinitionCase(_, valueName, valueDefinition, (_, inValue)) =>
      val args = valueDefinition.inputTypes.map(_._1.toCamelCase).mkString(" ")
      val body = valueDefinition.body._2
      s"let ${valueName.toCamelCase}$args = $body in $inValue"
    case LetRecursionCase(_, valueDefinitions, (_, inValue)) =>
      val defs = valueDefinitions
        .map { case (name, defn) =>
          val args = defn.inputTypes.map(_._1.toCamelCase).mkString(" ")
          val body = defn.body._2
          s"${name.toCamelCase}$args = $body"
        }
        .mkString("; ")
      s"let $defs in $inValue"
    case ListCase(_, elements)   => elements.map(_._2).mkString("[", ", ", "]")
    case LiteralCase(_, literal) => literal.toString
    case PatternMatchCase(_, (_, branchOutOn), cases) =>
      val casesStr = cases.map { case (pattern, (_, value)) => s"$pattern -> $value" }.mkString("; ")
      s"case $branchOutOn of $casesStr"
    case RecordCase(_, fields) =>
      fields
        .map { case (fieldName, (_, fieldValue)) => s"${fieldName.toCamelCase} = $fieldValue" }
        .mkString("{", ", ", "}")
    case ReferenceCase(_, name) =>
      Seq(
        Path.toString(Name.toTitleCase, ".", name.packagePath.toPath),
        Path.toString(Name.toTitleCase, ".", name.modulePath.toPath),
        name.localName.toCamelCase
      ).mkString(".")
    case TupleCase(_, elements) => elements.map(_._2).mkString("(", ", ", ")")
    case UnitCase(_)            => "()"
    case UpdateRecordCase(_, (_, valueToUpdate), fieldsToUpdate) =>
      val fieldsString = fieldsToUpdate
        .map { case (fieldName, (_, fieldValue)) => s"${fieldName.toCamelCase} = $fieldValue" }
        .mkString(", ")
      s"{ $valueToUpdate | $fieldsString }"
    case VariableCase(_, name) => name.toCamelCase
  }

  /**
   * Extract the argument list from a curried apply tree. It takes the two arguments of an apply and returns a tuple of
   * the function and a list of arguments.
   *
   * {{{
   *  assert(Apply((), f,a).uncurryApply(b) == (f, List(a, b)))
   * }}}
   */
  def uncurryApply[TB >: TA, VB >: VA](lastArg: Value[TB, VB]): (Value[TB, VB], scala.List[Value[TB, VB]]) =
    self match {
      case Value(ApplyCase(_, nestedFun, nestedArg)) =>
        val (f, initArgs) = nestedFun.uncurryApply(nestedArg)
        (f, initArgs :+ lastArg)
      case _ => (self, scala.List(lastArg))
    }
}

object Value extends ValueConstructors with PatternConstructors with DefinitionConstructors {
  import ValueCase._

  type RawValue = Value[Any, Any]
  val RawValue: Value.type = Value

  type TypedValue = Value[Any, UType]
  val TypedValue: Value.type = Value
  object Apply {
    def apply[TA, VA](
        attributes: VA,
        function: Value[TA, VA],
        argument: Value[TA, VA],
        arguments: Value[TA, VA]*
    ): Value[TA, VA] =
      arguments.foldLeft(Value(ApplyCase(attributes, function, argument))) { case (acc, arg) =>
        Value(ApplyCase(acc.attributes, acc, arg))
      }

    def unapply[TA, VA](value: Value[TA, VA]): Option[(VA, Value[TA, VA], Value[TA, VA])] = value.caseValue match {
      case ApplyCase(attributes, function, argument) => Some((attributes, function, argument))
      case _                                         => None
    }

    object Raw {

      def apply(function: RawValue, argument: RawValue, arguments: RawValue*): RawValue =
        arguments.foldLeft(Value(ApplyCase(function.attributes, function, argument))) { case (acc, arg) =>
          Value(ApplyCase(acc.attributes, acc, arg))
        }

      def unapply(value: RawValue): Option[(RawValue, RawValue)] = value.caseValue match {
        case ApplyCase(attributes, function, argument) => Some((function, argument))
        case _                                         => None
      }
    }

    object Typed {

      def apply(tpe: UType, function: TypedValue, argument: TypedValue, arguments: TypedValue*): TypedValue =
        arguments.foldLeft(Value(ApplyCase(tpe, function, argument))) { case (acc, arg) =>
          Value(ApplyCase(acc.attributes, acc, arg))
        }

      def apply(function: TypedValue, argument: TypedValue, arguments: TypedValue*): TypedValue =
        Typed(function.attributes, function, argument, arguments: _*)

      def unapply(value: TypedValue): Option[(UType, TypedValue, TypedValue)] = value.caseValue match {
        case ApplyCase(attributes, function, argument) => Some((attributes, function, argument))
        case _                                         => None
      }
    }
  }

  object Constructor {
    def apply[A](attributes: A, name: String): Value[Nothing, A] =
      Value(ConstructorCase(attributes, FQName.fromString(name)))

    def apply[A](attributes: A, name: FQName): Value[Nothing, A] = Value(ConstructorCase(attributes, name))

    def unapply[A](value: Value[Nothing, A]): Option[(A, FQName)] = value.caseValue match {
      case ConstructorCase(attributes, name) => Some((attributes, name))
      case _                                 => None
    }

    object Raw {
      @inline def apply(name: String): Value[Nothing, Any] = Constructor((), name)
      @inline def apply(name: FQName): Value[Nothing, Any] = Constructor((), name)
      def unapply(value: Value[Nothing, Any]): Option[FQName] = value.caseValue match {
        case ConstructorCase(_, name) => Some(name)
        case _                        => None
      }
    }
    object Typed {
      def apply(name: FQName, ascribedType: UType): TypedValue   = Constructor(ascribedType, name)
      def apply(fqName: String, ascribedType: UType): TypedValue = Constructor(ascribedType, FQName.fromString(fqName))
      def apply(ascribedType: UType, name: FQName): TypedValue   = Constructor(ascribedType, name)
      def apply(ascribedType: UType, fqName: String): TypedValue = Constructor(ascribedType, FQName.fromString(fqName))

      def unapply(value: TypedValue): Option[(UType, FQName)] = value.caseValue match {
        case ConstructorCase(attributes, name) => Some((attributes, name))
        case _                                 => None
      }
    }
  }

  object Destructure {
    def apply[TA, VA](
        attributes: VA,
        pattern: Pattern[VA],
        valueToDestruct: Value[TA, VA],
        inValue: Value[TA, VA]
    ): Value[TA, VA] =
      Value(DestructureCase(attributes, pattern, valueToDestruct, inValue))

    def unapply[TA, VA](value: Value[TA, VA]): Option[(VA, Pattern[VA], Value[TA, VA], Value[TA, VA])] =
      value.caseValue match {
        case DestructureCase(attributes, pattern, valueToDestruct, inValue) =>
          Some((attributes, pattern, valueToDestruct, inValue))
        case _ => None
      }

    object Raw {
      def apply(pattern: UPattern, valueToDestruct: RawValue, inValue: RawValue): RawValue =
        Value(DestructureCase((), pattern, valueToDestruct, inValue))

      def unapply(value: RawValue): Option[(UPattern, RawValue, RawValue)] =
        value.caseValue match {
          case DestructureCase(_, pattern, valueToDestruct, inValue) => Some((pattern, valueToDestruct, inValue))
          case _                                                     => None
        }
    }

    object Typed {
      def apply(tpe: UType, pattern: Pattern[UType], valueToDestruct: TypedValue, inValue: TypedValue): TypedValue =
        Value(DestructureCase(tpe, pattern, valueToDestruct, inValue))

      def apply(pattern: Pattern[UType], valueToDestruct: TypedValue, inValue: TypedValue): TypedValue =
        Value(DestructureCase(inValue.attributes, pattern, valueToDestruct, inValue))

      def unapply(value: TypedValue): Option[(UType, Pattern[UType], TypedValue, TypedValue)] =
        value.caseValue match {
          case DestructureCase(attributes, pattern, valueToDestruct, inValue) =>
            Some((attributes, pattern, valueToDestruct, inValue))
          case _ => None
        }
    }
  }
  object Field {
    def apply[TA, VA](attributes: VA, target: Value[TA, VA], name: Name): Value[TA, VA] =
      Value(FieldCase(attributes, target, name))

    def apply[TA, VA](attributes: VA, target: Value[TA, VA], name: String): Value[TA, VA] =
      Value(FieldCase(attributes, target, Name.fromString(name)))

    def unapply[TA, VA](value: Value[TA, VA]): Option[(VA, Value[TA, VA], Name)] = value.caseValue match {
      case FieldCase(attributes, target, name) => Some((attributes, target, name))
      case _                                   => None
    }

    object Raw {
      def apply(target: RawValue, name: Name): RawValue =
        Value(FieldCase(target.attributes, target, name))

      def apply(target: RawValue, name: String): RawValue =
        Value(FieldCase(target.attributes, target, Name.fromString(name)))

      def unapply(value: RawValue): Option[(RawValue, Name)] = value.caseValue match {
        case FieldCase(attributes, target, name) => Some((target, name))
        case _                                   => None
      }
    }

    object Typed {
      def apply(tpe: UType, target: TypedValue, name: Name): TypedValue =
        Value(FieldCase(tpe, target, name))

      def apply(tpe: UType, target: TypedValue, name: String): TypedValue =
        Value(FieldCase(tpe, target, Name.fromString(name)))

      def unapply(value: TypedValue): Option[(UType, TypedValue, Name)] = value.caseValue match {
        case FieldCase(attributes, target, name) => Some((attributes, target, name))
        case _                                   => None
      }
    }
  }

  object FieldFunction {
    def apply[VA](attributes: VA, name: String): Value[Nothing, VA] = Value(
      FieldFunctionCase(attributes, Name.fromString(name))
    )
    def apply[VA](attributes: VA, name: Name): Value[Nothing, VA] = Value(FieldFunctionCase(attributes, name))

    def unapply[VA](value: Value[Nothing, VA]): Option[(VA, Name)] = value.caseValue match {
      case FieldFunctionCase(attributes, name) => Some((attributes, name))
      case _                                   => None
    }

    object Raw {
      @inline def apply(name: String): RawValue = FieldFunction((), name)
      @inline def apply(name: Name): RawValue   = FieldFunction((), name)

      def unapply(value: RawValue): Option[Name] = value.caseValue match {
        case FieldFunctionCase(_, name) => Some(name)
        case _                          => None
      }
    }

    object Typed {
      def apply(tpe: UType, name: String): TypedValue = FieldFunction(tpe, name)
      def apply(tpe: UType, name: Name): TypedValue   = FieldFunction(tpe, name)

      def unapply(value: TypedValue): Option[(UType, Name)] = value.caseValue match {
        case FieldFunctionCase(attributes, name) => Some((attributes, name))
        case _                                   => None
      }
    }
  }

  object IfThenElse {
    def apply[TA, VA](
        attributes: VA,
        condition: Value[TA, VA],
        thenBranch: Value[TA, VA],
        elseBranch: Value[TA, VA]
    ): Value[TA, VA] =
      Value(IfThenElseCase(attributes, condition, thenBranch, elseBranch))

    def unapply[TA, VA](value: Value[TA, VA]): Option[(VA, Value[TA, VA], Value[TA, VA], Value[TA, VA])] =
      value.caseValue match {
        case IfThenElseCase(attributes, condition, thenValue, elseValue) =>
          Some((attributes, condition, thenValue, elseValue))
        case _ => None
      }

    object Raw {
      def apply(condition: RawValue, thenValue: RawValue, elseValue: RawValue): RawValue =
        Value(IfThenElseCase((), condition, thenValue, elseValue))

      def unapply(value: RawValue): Option[(RawValue, RawValue, RawValue)] =
        value.caseValue match {
          case IfThenElseCase(_, condition, thenValue, elseValue) => Some((condition, thenValue, elseValue))
          case _                                                  => None
        }
    }

    object Typed {
      def apply(
          tpe: UType,
          condition: TypedValue,
          thenBranch: TypedValue,
          elseBranch: TypedValue
      ): TypedValue =
        Value(IfThenElseCase(tpe, condition, thenBranch, elseBranch))

      def apply(condition: TypedValue, thenBranch: TypedValue, elseBranch: TypedValue): TypedValue =
        Value(IfThenElseCase(thenBranch.attributes, condition, thenBranch, elseBranch))

      def unapply(value: TypedValue): Option[(UType, TypedValue, TypedValue, TypedValue)] =
        value.caseValue match {
          case IfThenElseCase(attributes, condition, thenValue, elseValue) =>
            Some((attributes, condition, thenValue, elseValue))
          case _ => None
        }
    }
  }
  object Lambda {
    def apply[TA, VA](attributes: VA, argumentPattern: Pattern[VA], body: Value[TA, VA]): Value[TA, VA] =
      Value(LambdaCase(attributes, argumentPattern, body))

    def unapply[TA, VA](value: Value[TA, VA]): Option[(VA, Pattern[VA], Value[TA, VA])] = value.caseValue match {
      case LambdaCase(attributes, argumentPattern, body) => Some((attributes, argumentPattern, body))
      case _                                             => None
    }

    object Raw {
      def apply(argumentPattern: Pattern[Any], body: RawValue): RawValue =
        Value(LambdaCase(body.attributes, argumentPattern, body))

      def unapply(value: RawValue): Option[(Pattern[Any], RawValue)] = value.caseValue match {
        case LambdaCase(attributes, argumentPattern, body) => Some((argumentPattern, body))
        case _                                             => None
      }
    }

    object Typed {
      def apply(
          tpe: UType,
          argumentPattern: Pattern[UType],
          body: TypedValue
      ): TypedValue =
        Value(LambdaCase(tpe, argumentPattern, body))

      def unapply(value: TypedValue): Option[(UType, Pattern[Any], TypedValue)] =
        value.caseValue match {
          case LambdaCase(attributes, argumentPattern, body) => Some((attributes, argumentPattern, body))
          case _                                             => None
        }
    }
  }

  object LetDefinition {
    def apply[TA, VA](
        attributes: VA,
        name: Name,
        valueDefinition: Definition[TA, VA],
        inValue: Value[TA, VA]
    ): Value[TA, VA] =
      Value(LetDefinitionCase(attributes, name, valueDefinition.toCase, inValue))

    def apply[TA, VA](
        attributes: VA,
        name: Name,
        valueDefinition: Definition.Case[TA, VA, Type, Value[TA, VA]],
        inValue: Value[TA, VA]
    ): Value[TA, VA] =
      Value(LetDefinitionCase(attributes, name, valueDefinition, inValue))

    def apply[TA, VA](
        attributes: VA,
        name: String,
        valueDefinition: Definition[TA, VA],
        inValue: Value[TA, VA]
    ): Value[TA, VA] =
      Value(LetDefinitionCase(attributes, Name.fromString(name), valueDefinition.toCase, inValue))

    def apply[TA, VA](
        attributes: VA,
        name: String,
        valueDefinition: Definition.Case[TA, VA, Type, Value[TA, VA]],
        inValue: Value[TA, VA]
    ): Value[TA, VA] =
      Value(LetDefinitionCase(attributes, Name.fromString(name), valueDefinition, inValue))

    object Raw {
      def apply(name: Name, valueDefinition: Definition.Raw, inValue: RawValue): RawValue =
        Value(LetDefinitionCase(inValue.attributes, name, valueDefinition.toCase, inValue))

      def apply(name: String, valueDefinition: Definition.Raw, inValue: RawValue): RawValue =
        Value(LetDefinitionCase(inValue.attributes, Name.fromString(name), valueDefinition.toCase, inValue))
    }

    object Typed {
      def apply(
          tpe: UType,
          name: Name,
          valueDefinition: Definition.Typed,
          inValue: TypedValue
      ): TypedValue =
        Value(LetDefinitionCase(tpe, name, valueDefinition.toCase, inValue))

      def apply(tpe: UType, name: String, valueDefinition: Definition.Typed, inValue: TypedValue): TypedValue =
        Value(LetDefinitionCase(tpe, Name.fromString(name), valueDefinition.toCase, inValue))

      def apply(
          name: Name,
          valueDefinition: Definition.Typed,
          inValue: TypedValue
      ): TypedValue =
        Value(LetDefinitionCase(inValue.attributes, name, valueDefinition.toCase, inValue))

      def apply(name: String, valueDefinition: Definition.Typed, inValue: TypedValue): TypedValue =
        Value(LetDefinitionCase(inValue.attributes, Name.fromString(name), valueDefinition.toCase, inValue))

    }

    final case class Unbound[+TA, +VA](name: Name, valueDefinition: Definition[TA, VA]) {
      def bind[TB >: TA, VB >: VA](value: Value[TB, VB]): Value[TB, VB] =
        LetDefinition(value.attributes, name, valueDefinition, value)

      def in[TB >: TA, VB >: VA](value: Value[TB, VB]): Value[TB, VB] =
        LetDefinition(value.attributes, name, valueDefinition, value)

      override def toString(): String = {
        val args = valueDefinition.inputTypes.map(_._1.toCamelCase).mkString(" ")
        val body = valueDefinition.body.toString()
        s"let ${name.toCamelCase}$args = $body"
      }
    }
  }

  object LetRecursion {
    def apply[TA, VA](
        attributes: VA,
        valueDefinitions: Map[Name, Definition[TA, VA]],
        inValue: Value[TA, VA]
    ): Value[TA, VA] = Value(
      LetRecursionCase(attributes, valueDefinitions.map { case (n, d) => (n, d.toCase) }, inValue)
    )

    def apply[TA, VA](attributes: VA, valueDefinitions: (String, Definition[TA, VA])*)(
        inValue: Value[TA, VA]
    ): Value[TA, VA] =
      Value(
        LetRecursionCase(
          attributes,
          valueDefinitions.map { case (n, d) => (Name.fromString(n), d.toCase) }.toMap,
          inValue
        )
      )

    object Raw {
      def apply(valueDefinitions: Map[Name, Definition.Raw], inValue: RawValue): RawValue =
        Value(LetRecursionCase(inValue.attributes, valueDefinitions.map { case (n, d) => (n, d.toCase) }, inValue))

      def apply(valueDefinitions: (String, Definition.Raw)*)(inValue: RawValue): RawValue =
        Value(
          LetRecursionCase(
            inValue.attributes,
            valueDefinitions.map { case (n, d) => (Name.fromString(n), d.toCase) }.toMap,
            inValue
          )
        )
    }

    object Typed {
      def apply(
          tpe: UType,
          valueDefinitions: Map[Name, Definition.Typed],
          inValue: TypedValue
      ): TypedValue =
        Value(
          LetRecursionCase(
            tpe,
            valueDefinitions.map { case (n, d) => (n, d.toCase) },
            inValue
          )
        )

      def apply(tpe: UType, valueDefinitions: (String, Definition[Any, UType])*)(
          inValue: TypedValue
      ): TypedValue =
        Value(
          LetRecursionCase(
            tpe,
            valueDefinitions.map { case (n, d) => (Name.fromString(n), d.toCase) }.toMap,
            inValue
          )
        )

      def apply(
          valueDefinitions: Map[Name, Definition.Typed],
          inValue: TypedValue
      ): TypedValue =
        Value(
          LetRecursionCase(
            inValue.attributes,
            valueDefinitions.map { case (n, d) => (n, d.toCase) },
            inValue
          )
        )

      def apply(valueDefinitions: (String, Definition[Any, UType])*)(
          inValue: TypedValue
      ): TypedValue =
        Value(
          LetRecursionCase(
            inValue.attributes,
            valueDefinitions.map { case (n, d) => (Name.fromString(n), d.toCase) }.toMap,
            inValue
          )
        )
    }
  }

  object List {
    def apply[TA, VA](attributes: VA, elements: Chunk[Value[TA, VA]]): Value[TA, VA] =
      Value(ListCase(attributes, elements))

    def apply[TA, VA](attributes: VA, elements: Value[TA, VA]*): Value[TA, VA] =
      apply(attributes, Chunk.fromIterable(elements))

    def unapply[TA, VA](value: Value[TA, VA]): Option[(VA, Chunk[Value[TA, VA]])] = value.caseValue match {
      case ListCase(attributes, elements) => Some((attributes, elements))
      case _                              => None
    }

    object Raw {
      def apply(elements: Chunk[RawValue]): RawValue = Value(ListCase((), elements))
      def apply(elements: RawValue*): RawValue       = Value(ListCase((), Chunk.fromIterable(elements)))

      def unapply(value: RawValue): Option[Chunk[RawValue]] = value.caseValue match {
        case ListCase(_, elements) => Some(elements)
        case _                     => None
      }
    }

    object Typed {
      def apply(tpe: UType, elements: Chunk[TypedValue]): TypedValue = Value(ListCase(tpe, elements))

      def apply(elements: NonEmptyChunk[TypedValue]): TypedValue = {
        val tpe = zio.morphir.ir.sdk.List.listType(elements.head.attributes)
        Value(ListCase(tpe, elements))
      }

      def apply(tpe: UType, elements: TypedValue*): TypedValue =
        Value(ListCase(tpe, Chunk.fromIterable(elements)))

      def apply(head: TypedValue, tail: TypedValue*): TypedValue =
        Value(ListCase(head.attributes, Chunk.fromIterable(head +: tail)))

      def unapply(value: TypedValue): Option[(UType, Chunk[TypedValue])] = value.caseValue match {
        case ListCase(tpe, elements) => Some((tpe, elements))
        case _                       => None
      }
    }
  }

  object Literal {
    def apply[VA, A](attributes: VA, literal: Lit[A]): Value[Nothing, VA] =
      Value(LiteralCase(attributes, literal))

    def unapply[VA](value: Value[Nothing, VA]): Option[(VA, Lit[Any])] = value.caseValue match {
      case LiteralCase(attributes, literal) => Some((attributes, literal))
      case _                                => None
    }
    object Raw {
      def apply[A](literal: Lit[A]): RawValue = Literal((), literal)

      def unapply(value: RawValue): Option[Lit[Any]] = value.caseValue match {
        case LiteralCase(_, literal) => Some(literal)
        case _                       => None
      }
    }

    object Typed {
      def apply[A](value: Lit[A]): TypedValue = Literal(value.inferredType, value)

      def unapply(value: TypedValue): Option[Lit[Any]] = value.caseValue match {
        case LiteralCase(_, literal) => Some(literal)
        case _                       => None
      }
    }
  }

  object PatternMatch {
    def apply[TA, VA](
        attributes: VA,
        target: Value[TA, VA],
        cases: Chunk[(Pattern[VA], Value[TA, VA])]
    ): Value[TA, VA] =
      Value(PatternMatchCase(attributes, target, cases))

    def apply[TA, VA](attributes: VA, target: Value[TA, VA], cases: (Pattern[VA], Value[TA, VA])*): Value[TA, VA] =
      Value(PatternMatchCase(attributes, target, Chunk.fromIterable(cases)))

    def unapply[TA, VA](value: Value[TA, VA]): Option[(VA, Value[TA, VA], Chunk[(Pattern[VA], Value[TA, VA])])] =
      value.caseValue match {
        case PatternMatchCase(attributes, target, cases) => Some((attributes, target, cases))
        case _                                           => None
      }

    object Raw {
      def apply(branchOutOn: RawValue, cases: Chunk[(UPattern, RawValue)]): RawValue =
        Value(PatternMatchCase((), branchOutOn, cases))
      def apply(target: RawValue, cases: (Pattern[Any], RawValue)*): RawValue =
        Value(PatternMatchCase((), target, Chunk.fromIterable(cases)))

      def unapply(value: RawValue): Option[(RawValue, Chunk[(Pattern[Any], RawValue)])] =
        value.caseValue match {
          case PatternMatchCase(_, target, cases) => Some((target, cases))
          case _                                  => None
        }
    }

    object Typed {
      def apply(tpe: UType, target: TypedValue, cases: Chunk[(Pattern[UType], TypedValue)]): TypedValue =
        Value(PatternMatchCase(tpe, target, cases))

      def unapply(value: TypedValue): Option[(UType, TypedValue, Chunk[(Pattern[UType], TypedValue)])] =
        value.caseValue match {
          case PatternMatchCase(tpe, target, cases) => Some((tpe, target, cases))
          case _                                    => None
        }
    }
  }
  object Record {
    def apply[TA, VA](attributes: VA, fields: Chunk[(Name, Value[TA, VA])]): Value[TA, VA] =
      Value(RecordCase(attributes, fields))

    def apply[TA, VA](attributes: VA, fields: (String, Value[TA, VA])*): Value[TA, VA] =
      Value(
        RecordCase(attributes, Chunk.fromIterable(fields.map { case (name, value) => (Name.fromString(name), value) }))
      )

    def apply[TA, VA](
        attributes: VA,
        firstField: (Name, Value[TA, VA]),
        otherFields: (Name, Value[TA, VA])*
    ): Value[TA, VA] =
      Value(RecordCase(attributes, firstField +: Chunk.fromIterable(otherFields)))

    def apply[TA, VA](attributes: VA, fields: Map[String, Value[TA, VA]]): Value[TA, VA] = {
      val fieldsChunk = Chunk.fromIterable(fields.map { case (name, value) => (Name.fromString(name), value) })
      Value(RecordCase(attributes, fieldsChunk))
    }

    def fromMap[TA, VA](attributes: VA, fields: Map[Name, Value[TA, VA]]): Value[TA, VA] =
      Value(RecordCase(attributes, Chunk.fromIterable(fields.map { case (name, value) => (name, value) })))

    def unapply[TA, VA](value: Value[TA, VA]): Option[(VA, Chunk[(Name, Value[TA, VA])])] = value.caseValue match {
      case RecordCase(attributes, fields) => Some((attributes, fields))
      case _                              => None
    }

    object Raw {
      def apply(fields: Chunk[(Name, RawValue)]): RawValue = Value(RecordCase((), fields))
      def apply(firstField: (Name, RawValue), otherFields: (Name, RawValue)*): RawValue =
        Record((), firstField, otherFields: _*)

      def apply(fields: (String, RawValue)*): RawValue = Record((), fields: _*)

      def apply(fields: Map[String, RawValue]): RawValue = Record((), fields)

      def unapply(value: RawValue): Option[Chunk[(Name, RawValue)]] = value.caseValue match {
        case RecordCase(_, fields) => Some(fields)
        case _                     => None
      }
    }

    object Typed {
      def apply(tpe: UType, fields: Chunk[(Name, TypedValue)]): TypedValue =
        Value(RecordCase(tpe, fields))

      def apply(tpe: UType, fields: (String, TypedValue)*): TypedValue =
        Value(RecordCase(tpe, Chunk.fromIterable(fields.map { case (name, value) => (Name.fromString(name), value) })))

      def apply(tpe: UType, fields: Map[String, TypedValue]): TypedValue =
        Value(RecordCase(tpe, Chunk.fromIterable(fields.map { case (name, value) => (Name.fromString(name), value) })))

      def apply(fields: (String, TypedValue)*): TypedValue = {
        val fieldTypes = Chunk.fromIterable(fields.map { case (name, value) => Type.field(name, value.attributes) })
        val tpe        = Type.record(fieldTypes)
        Value(RecordCase(tpe, Chunk.fromIterable(fields.map { case (name, value) => (Name.fromString(name), value) })))
      }

      def unapply(value: TypedValue): Option[(UType, Chunk[(Name, TypedValue)])] = value.caseValue match {
        case RecordCase(tpe, fields) => Some((tpe, fields))
        case _                       => None
      }
    }
  }

  object Reference {
    def apply[A](attributes: A, name: String): Value[Nothing, A] =
      Value(ReferenceCase(attributes, FQName.fromString(name)))

    def apply[A](attributes: A, name: FQName): Value[Nothing, A] = Value(ReferenceCase(attributes, name))

    def apply[A](attributes: A, packageName: String, moduleName: String, localName: String): Value[Nothing, A] =
      Value(ReferenceCase(attributes, FQName.fqn(packageName, moduleName, localName)))

    def unapply[A](value: Value[Nothing, A]): Option[(A, FQName)] = value.caseValue match {
      case ReferenceCase(attributes, name) => Some((attributes, name))
      case _                               => None
    }

    object Raw {
      @inline def apply(name: String): RawValue = Reference((), name)
      @inline def apply(name: FQName): RawValue = Reference((), name)
      @inline def apply(packageName: String, moduleName: String, localName: String): RawValue =
        Value(ReferenceCase((), FQName.fqn(packageName, moduleName, localName)))

      def unapply(value: Value[Nothing, Any]): Option[FQName] = value.caseValue match {
        case ReferenceCase(_, name) => Some(name)
        case _                      => None
      }
    }

    object Typed {
      @inline def apply(tpe: UType, name: String): TypedValue = Reference(tpe, name)
      @inline def apply(tpe: UType, name: FQName): TypedValue = Reference(tpe, name)
      @inline def apply(tpe: UType, packageName: String, moduleName: String, localName: String): TypedValue =
        Reference(tpe, FQName.fqn(packageName, moduleName, localName))

      def unapply(value: TypedValue): Option[(UType, FQName)] = value.caseValue match {
        case ReferenceCase(tpe, name) => Some((tpe, name))
        case _                        => None
      }
    }
  }

  object Tuple {
    def apply[VA](attributes: VA): Value[Nothing, VA] = Value(TupleCase(attributes, Chunk.empty))

    def apply[TA, VA](attributes: VA, elements: Chunk[Value[TA, VA]]): Value[TA, VA] = Value(
      TupleCase(attributes, elements)
    )

    def apply[TA, VA](attributes: VA, element: Value[TA, VA], otherElements: Value[TA, VA]*): Value[TA, VA] =
      apply(attributes, element +: Chunk.fromIterable(otherElements))

    def unapply[TA, VA](value: Value[TA, VA]): Option[(VA, Chunk[Value[TA, VA]])] = value.caseValue match {
      case TupleCase(attributes, elements) => Some((attributes, elements))
      case _                               => None
    }

    object Raw {
      def apply(elements: Chunk[RawValue]): RawValue = Tuple((), elements)
      def apply(elements: RawValue*): RawValue       = Tuple((), Chunk.fromIterable(elements))

      def unapply(value: RawValue): Option[Chunk[RawValue]] = value.caseValue match {
        case TupleCase(_, elements) => Some(elements)
        case _                      => None
      }
    }

    object Typed {
      def apply(elements: Chunk[TypedValue]): TypedValue = {
        val tupleType = Type.tuple(elements.map(_.attributes))
        Value(TupleCase(tupleType, elements))
      }

      def apply(elements: TypedValue*): TypedValue = apply(Chunk.fromIterable(elements))

      def unapply(value: TypedValue): Option[(UType, Chunk[TypedValue])] = value.caseValue match {
        case TupleCase(attributes, elements) => Some((attributes, elements))
        case _                               => None
      }
    }
  }

  object Unit {
    def apply[VA](attributes: VA): Value[Nothing, VA] = Value(UnitCase(attributes))

    def unapply[VA](value: Value[Nothing, VA]): Option[VA] = value match {
      case Value(UnitCase(attributes)) => Some(attributes)
      case _                           => None
    }

    object Raw {
      def apply(): RawValue = Value(UnitCase(()))
      def unapply(value: RawValue): Option[scala.Unit] = value match {
        case Value(UnitCase(())) => Some(())
        case _                   => None
      }
    }

    object Typed {
      def apply: TypedValue             = Value(UnitCase(Type.unit))
      def apply(tpe: UType): TypedValue = Value(UnitCase(tpe))

      def unapply(value: TypedValue): Option[UType] = value match {
        case Value(UnitCase(attributes)) => Some(attributes)
        case _                           => None
      }
    }
  }

  object UpdateRecord {
    def apply[TA, VA](
        attributes: VA,
        valueToUpdate: Value[TA, VA],
        fields: Chunk[(Name, Value[TA, VA])]
    ): Value[TA, VA] =
      Value(UpdateRecordCase(attributes, valueToUpdate, fields))

    def apply[TA, VA](attributes: VA, valueToUpdate: Value[TA, VA], fields: (String, Value[TA, VA])*): Value[TA, VA] =
      Value(
        UpdateRecordCase(
          attributes,
          valueToUpdate,
          Chunk.fromIterable(fields.map { case (name, value) => (Name.fromString(name), value) })
        )
      )

    def unapply[TA, VA](value: Value[TA, VA]): Option[(VA, Value[TA, VA], Chunk[(Name, Value[TA, VA])])] =
      value.caseValue match {
        case UpdateRecordCase(attributes, value, fields) => Some((attributes, value, fields))
        case _                                           => None
      }

    object Raw {
      def apply(record: RawValue, fields: Chunk[(Name, RawValue)]): RawValue =
        Value(UpdateRecordCase((), record, fields))

      def apply(valueToUpdate: RawValue, fields: (String, RawValue)*): RawValue =
        Value(
          UpdateRecordCase(
            (),
            valueToUpdate,
            Chunk.fromIterable(fields.map { case (name, value) => (Name.fromString(name), value) })
          )
        )

      def unapply(value: RawValue): Option[(RawValue, Chunk[(Name, RawValue)])] = value.caseValue match {
        case UpdateRecordCase(_, record, fields) => Some((record, fields))
        case _                                   => None
      }
    }

    object Typed {
      def apply(tpe: UType, valueToUpdate: TypedValue, fields: Chunk[(Name, TypedValue)]): TypedValue =
        Value(UpdateRecordCase(tpe, valueToUpdate, fields))

      def apply(tpe: UType, valueToUpdate: TypedValue, fields: (String, TypedValue)*): TypedValue =
        Value(
          UpdateRecordCase(
            tpe,
            valueToUpdate,
            Chunk.fromIterable(fields.map { case (name, value) => (Name.fromString(name), value) })
          )
        )

      def unapply(value: TypedValue): Option[(UType, TypedValue, Chunk[(Name, TypedValue)])] =
        value.caseValue match {
          case UpdateRecordCase(tpe, value, fields) => Some((tpe, value, fields))
          case _                                    => None
        }
    }
  }

  object Variable {
    def apply[VA](attributes: VA, name: Name): Value[Nothing, VA] =
      Value(VariableCase(attributes, name))
    def apply[VA](attributes: VA, name: String): Value[Nothing, VA] =
      Value(VariableCase(attributes, Name.fromString(name)))

    def unapply[VA](value: Value[Nothing, VA]): Option[(VA, Name)] = value.caseValue match {
      case VariableCase(attributes, name) => Some((attributes, name))
      case _                              => None
    }

    object Raw {
      @inline def apply(name: Name): RawValue   = Variable((), name)
      @inline def apply(name: String): RawValue = Variable((), name)
    }

    object Typed {
      @inline def apply(tpe: UType, name: Name): TypedValue   = Variable(tpe, name)
      @inline def apply(tpe: UType, name: String): TypedValue = Variable(tpe, name)
      @inline def apply(name: String, tpe: UType): TypedValue = Variable(tpe, name)
      @inline def apply(name: Name, tpe: UType): TypedValue   = Variable(tpe, name)

      def unapply(value: TypedValue): Option[(UType, Name)] = value.caseValue match {
        case VariableCase(tpe, name) => Some((tpe, name))
        case _                       => None
      }
    }
  }

  implicit class StringExtensions(private val self: String) extends AnyVal {
    def as(tpe: Type.UType): TypedValue = Variable.Typed(self, tpe)
    def :=(value: TypedValue): LetDefinition.Unbound[Any, UType] =
      LetDefinition.Unbound(Name.fromString(self), Definition.fromTypedValue(value))

    def :=(value: Int): LetDefinition.Unbound[Any, UType] =
      LetDefinition.Unbound(Name.fromString(self), Definition.fromLiteral(Lit.int(value)))

    def :=(value: Long): LetDefinition.Unbound[Any, UType] =
      LetDefinition.Unbound(Name.fromString(self), Definition.fromLiteral(Lit.long(value)))

    def :=(value: Float): LetDefinition.Unbound[Any, UType] =
      LetDefinition.Unbound(Name.fromString(self), Definition.fromLiteral(Lit.float(value)))

    def :=(value: Double): LetDefinition.Unbound[Any, UType] =
      LetDefinition.Unbound(Name.fromString(self), Definition.fromLiteral(Lit.double(value)))

    def :=(value: Boolean): LetDefinition.Unbound[Any, UType] =
      LetDefinition.Unbound(Name.fromString(self), Definition.fromLiteral(Lit.boolean(value)))

    def :=(value: String): LetDefinition.Unbound[Any, UType] =
      LetDefinition.Unbound(Name.fromString(self), Definition.fromLiteral(Lit.string(value)))

  }

  implicit class RawValueExtensions(private val self: RawValue) extends AnyVal {

    /**
     * Ascribe the given type to this `RawValue` and all its children.
     * ===NOTE===
     * This is a recursive operation and all children of this `RawValue` will also be ascribed with the given value.
     */
    def :>(ascribedType: UType): TypedValue = self.mapAttributes(identity, _ => ascribedType)

    def toValDef(returnType: UType): Definition[Any, UType] = Definition(returnType, self :> returnType)
  }

  implicit class TypedValueExtensions(private val self: TypedValue) extends AnyVal {

    /**
     * Ascribe the given type of this `TypedValue` and all its children.
     * ===NOTE===
     * This is NOT a recursive operation and it will only ascribe the type of this value and not its children.
     */
    def :>(ascribedType: UType): TypedValue = self.caseValue match {
      case c @ ApplyCase(_, _, _)            => Value(c.copy(attributes = ascribedType))
      case c @ ConstructorCase(_, _)         => Value(c.copy(attributes = ascribedType))
      case c @ DestructureCase(_, _, _, _)   => Value(c.copy(attributes = ascribedType))
      case c @ FieldCase(_, _, _)            => Value(c.copy(attributes = ascribedType))
      case c @ FieldFunctionCase(_, _)       => Value(c.copy(attributes = ascribedType))
      case c @ IfThenElseCase(_, _, _, _)    => Value(c.copy(attributes = ascribedType))
      case c @ LambdaCase(_, _, _)           => Value(c.copy(attributes = ascribedType))
      case c @ LetDefinitionCase(_, _, _, _) => Value(c.copy(attributes = ascribedType))
      case c @ LetRecursionCase(_, _, _)     => Value(c.copy(attributes = ascribedType))
      case c @ ListCase(_, _)                => Value(c.copy(attributes = ascribedType))
      case c @ LiteralCase(_, _)             => Value(c.copy(attributes = ascribedType))
      case c @ PatternMatchCase(_, _, _)     => Value(c.copy(attributes = ascribedType))
      case c @ RecordCase(_, _)              => Value(c.copy(attributes = ascribedType))
      case c @ ReferenceCase(_, _)           => Value(c.copy(attributes = ascribedType))
      case c @ TupleCase(_, _)               => Value(c.copy(attributes = ascribedType))
      case c @ UnitCase(_)                   => Value(c.copy(attributes = ascribedType))
      case c @ UpdateRecordCase(_, _, _)     => Value(c.copy(attributes = ascribedType))
      case c @ VariableCase(_, _)            => Value(c.copy(attributes = ascribedType))
    }

    def toValDef(returnType: UType): Definition[Any, UType] = Definition(returnType, self)
    def toValDef: Definition[Any, UType]                    = Definition(self.attributes, self)
  }
}
