package zio.morphir.ir.value

import zio.Chunk
import zio.morphir.ir.Type.{Type, UType}
import zio.morphir.ir.{FQName, InferredTypeOf, Literal => Lit, Name, NativeFunction}

import scala.annotation.tailrec

sealed trait Value[+TA, +VA] { self =>
  import Value.{List => ListType, Unit => UnitType, _}

  def @@[LowerTA >: TA, UpperTA >: LowerTA, LowerVA >: VA, UpperVA >: LowerVA](
      aspect: ValueAspect[LowerTA, UpperTA, LowerVA, UpperVA]
  ): Value[LowerTA, LowerVA] =
    aspect(self)

  def attributes: VA

  def mapAttributes[TB, VB](f: TA => TB, g: VA => VB): Value[TB, VB] = self match {
    case t @ Apply(_, _, _) =>
      Apply(g(t.attributes), t.function.mapAttributes(f, g), t.argument.mapAttributes(f, g))
    case t @ Constructor(_, _) => Constructor(g(t.attributes), t.name)
    case t @ Destructure(_, _, _, _) =>
      Destructure(
        g(t.attributes),
        t.pattern.mapAttributes(g),
        t.valueToDestruct.mapAttributes(f, g),
        t.inValue.mapAttributes(f, g)
      )
    case t @ Field(_, _, _)      => Field(g(t.attributes), t.target.mapAttributes(f, g), t.name)
    case t @ FieldFunction(_, _) => FieldFunction(g(t.attributes), t.name)
    case t @ IfThenElse(_, _, _, _) =>
      IfThenElse(
        g(t.attributes),
        t.condition.mapAttributes(f, g),
        t.thenBranch.mapAttributes(f, g),
        t.elseBranch.mapAttributes(f, g)
      )
    case t @ Lambda(_, _, _) => Lambda(g(t.attributes), t.argumentPattern.mapAttributes(g), t.body.mapAttributes(f, g))
    case t @ LetDefinition(_, _, _, _) =>
      LetDefinition(g(t.attributes), t.valueName, t.valueDefinition.mapAttributes(f, g), t.inValue.mapAttributes(f, g))
    case t @ LetRecursion(_, _, _) =>
      LetRecursion(
        g(t.attributes),
        t.valueDefinitions.map { case (name, definition) => (name, definition.mapAttributes(f, g)) },
        t.inValue.mapAttributes(f, g)
      )
    case t @ ListType(_, _)       => ListType(g(t.attributes), t.elements.map(_.mapAttributes(f, g)))
    case t @ Literal(_, _)        => Literal(g(t.attributes), t.literal)
    case t @ NativeApply(_, _, _) => NativeApply(g(t.attributes), t.function, t.arguments.map(_.mapAttributes(f, g)))
    case t @ PatternMatch(_, _, _) =>
      PatternMatch(
        g(t.attributes),
        t.branchOutOn.mapAttributes(f, g),
        t.cases.map { case (p, v) => (p.mapAttributes(g), v.mapAttributes(f, g)) }
      )
    case t @ Record(_, _)    => Record(g(t.attributes), t.fields.map { case (n, v) => (n, v.mapAttributes(f, g)) })
    case t @ Reference(_, _) => Reference(g(t.attributes), t.name)
    case t @ Tuple(_, _)     => Tuple(g(t.attributes), t.elements.map(item => item.mapAttributes(f, g)))
    case t @ UnitType(_)     => UnitType(g(t.attributes))
    case t @ UpdateRecord(_, _, _) =>
      UpdateRecord(
        g(t.attributes),
        t.valueToUpdate.mapAttributes(f, g),
        t.fieldsToUpdate.map { case (n, v) => (n, v.mapAttributes(f, g)) }
      )
    case t @ Variable(_, _) => Variable(g(t.attributes), t.name)
  }

  def collectVariables: Set[Name] = foldLeft(Set.empty[Name]) {
    case (acc, Variable(_, name)) => acc + name
    case (acc, _)                 => acc
  }

  def collectReferences: Set[FQName] = foldLeft(Set.empty[FQName]) {
    case (acc, Reference(_, name)) => acc + name
    case (acc, _)                  => acc
  }

  // def indexedMapValue[VB](initial: Int)(f: (Int, VA) => VB): (Value[TA, VB], Int) = ???
  def rewrite[TB >: TA, VB >: VA](pf: PartialFunction[Value[TB, VB], Value[TB, VB]]): Value[TB, VB] =
    transform[TB, VB](v => pf.lift(v).getOrElse(v))

  def toRawValue: RawValue = mapAttributes(_ => (), _ => ())

  def transform[TB >: TA, VB >: VA](f: Value[TB, VB] => Value[TB, VB]): Value[TB, VB] = fold[Value[TB, VB]](
    applyCase = (attributes, function, arguments) => f(Apply(attributes, function, arguments)),
    constructorCase = (attributes, name) => f(Constructor(attributes, name)),
    destructureCase =
      (attributes, pattern, valueToDestruct, inValue) => f(Destructure(attributes, pattern, valueToDestruct, inValue)),
    fieldCase = (attributes, target, name) => f(Field(attributes, target, name)),
    fieldFunctionCase = (attributes, name) => f(FieldFunction(attributes, name)),
    ifThenElseCase =
      (attributes, condition, thenBranch, elseBranch) => f(IfThenElse(attributes, condition, thenBranch, elseBranch)),
    lambdaCase = (attributes, argumentPattern, body) => f(Lambda(attributes, argumentPattern, body)),
    letDefinitionCase = (attributes, valueName, valueDefinition, inValue) =>
      f(LetDefinition(attributes, valueName, valueDefinition.toDefinition, inValue)),
    letRecursionCase = (attributes, valueDefinitions, inValue) =>
      f(LetRecursion(attributes, valueDefinitions.map { case (n, d) => (n, d.toDefinition) }, inValue)),
    listCase = (attributes, elements) => f(ListType(attributes, elements)),
    literalCase = (attributes, literal) => f(Literal(attributes, literal)),
    nativeApplyCase = (attributes, function, arguments) => f(NativeApply(attributes, function, arguments)),
    patternMatchCase = (attributes, branchOutOn, cases) => f(PatternMatch(attributes, branchOutOn, cases)),
    recordCase = (attributes, fields) => f(Record(attributes, fields)),
    referenceCase = (attributes, name) => f(Reference(attributes, name)),
    tupleCase = (attributes, elements) => f(Tuple(attributes, elements)),
    unitCase = (attributes) => f(UnitType(attributes)),
    updateRecordCase =
      (attributes, valueToUpdate, fieldsToUpdate) => f(UpdateRecord(attributes, valueToUpdate, fieldsToUpdate)),
    variableCase = (attributes, name) => f(Variable(attributes, name))
  )

  def fold[Z](
      applyCase: (VA, Z, Z) => Z,
      constructorCase: (VA, FQName) => Z,
      destructureCase: (VA, Pattern[VA], Z, Z) => Z,
      fieldCase: (VA, Z, Name) => Z,
      fieldFunctionCase: (VA, Name) => Z,
      ifThenElseCase: (VA, Z, Z, Z) => Z,
      lambdaCase: (VA, Pattern[VA], Z) => Z,
      letDefinitionCase: (VA, Name, Definition.Case[TA, VA, Type, Z], Z) => Z,
      letRecursionCase: (VA, Map[Name, Definition.Case[TA, VA, Type, Z]], Z) => Z,
      listCase: (VA, Chunk[Z]) => Z,
      literalCase: (VA, Lit[_]) => Z,
      nativeApplyCase: (VA, NativeFunction, Chunk[Z]) => Z,
      patternMatchCase: (VA, Z, Chunk[(Pattern[VA], Z)]) => Z,
      recordCase: (VA, Chunk[(Name, Z)]) => Z,
      referenceCase: (VA, FQName) => Z,
      tupleCase: (VA, Chunk[Z]) => Z,
      unitCase: VA => Z,
      updateRecordCase: (VA, Z, Chunk[(Name, Z)]) => Z,
      variableCase: (VA, Name) => Z
  ): Z = self match {
    case Apply(attributes, function, arguments) =>
      applyCase(
        attributes,
        function.fold(
          applyCase,
          constructorCase,
          destructureCase,
          fieldCase,
          fieldFunctionCase,
          ifThenElseCase,
          lambdaCase,
          letDefinitionCase,
          letRecursionCase,
          listCase,
          literalCase,
          nativeApplyCase,
          patternMatchCase,
          recordCase,
          referenceCase,
          tupleCase,
          unitCase,
          updateRecordCase,
          variableCase
        ),
        arguments.fold(
          applyCase,
          constructorCase,
          destructureCase,
          fieldCase,
          fieldFunctionCase,
          ifThenElseCase,
          lambdaCase,
          letDefinitionCase,
          letRecursionCase,
          listCase,
          literalCase,
          nativeApplyCase,
          patternMatchCase,
          recordCase,
          referenceCase,
          tupleCase,
          unitCase,
          updateRecordCase,
          variableCase
        )
      )
    case Constructor(attributes, name) => constructorCase(attributes, name)
    case Destructure(attributes, pattern, valueToDestruct, inValue) =>
      destructureCase(
        attributes,
        pattern,
        valueToDestruct.fold(
          applyCase,
          constructorCase,
          destructureCase,
          fieldCase,
          fieldFunctionCase,
          ifThenElseCase,
          lambdaCase,
          letDefinitionCase,
          letRecursionCase,
          listCase,
          literalCase,
          nativeApplyCase,
          patternMatchCase,
          recordCase,
          referenceCase,
          tupleCase,
          unitCase,
          updateRecordCase,
          variableCase
        ),
        inValue.fold(
          applyCase,
          constructorCase,
          destructureCase,
          fieldCase,
          fieldFunctionCase,
          ifThenElseCase,
          lambdaCase,
          letDefinitionCase,
          letRecursionCase,
          listCase,
          literalCase,
          nativeApplyCase,
          patternMatchCase,
          recordCase,
          referenceCase,
          tupleCase,
          unitCase,
          updateRecordCase,
          variableCase
        )
      )
    case Field(attributes, target, name) =>
      fieldCase(
        attributes,
        target.fold(
          applyCase,
          constructorCase,
          destructureCase,
          fieldCase,
          fieldFunctionCase,
          ifThenElseCase,
          lambdaCase,
          letDefinitionCase,
          letRecursionCase,
          listCase,
          literalCase,
          nativeApplyCase,
          patternMatchCase,
          recordCase,
          referenceCase,
          tupleCase,
          unitCase,
          updateRecordCase,
          variableCase
        ),
        name
      )
    case FieldFunction(attributes, name) => fieldFunctionCase(attributes, name)
    case IfThenElse(attributes, condition, thenBranch, elseBranch) =>
      ifThenElseCase(
        attributes,
        condition.fold(
          applyCase,
          constructorCase,
          destructureCase,
          fieldCase,
          fieldFunctionCase,
          ifThenElseCase,
          lambdaCase,
          letDefinitionCase,
          letRecursionCase,
          listCase,
          literalCase,
          nativeApplyCase,
          patternMatchCase,
          recordCase,
          referenceCase,
          tupleCase,
          unitCase,
          updateRecordCase,
          variableCase
        ),
        thenBranch.fold(
          applyCase,
          constructorCase,
          destructureCase,
          fieldCase,
          fieldFunctionCase,
          ifThenElseCase,
          lambdaCase,
          letDefinitionCase,
          letRecursionCase,
          listCase,
          literalCase,
          nativeApplyCase,
          patternMatchCase,
          recordCase,
          referenceCase,
          tupleCase,
          unitCase,
          updateRecordCase,
          variableCase
        ),
        elseBranch.fold(
          applyCase,
          constructorCase,
          destructureCase,
          fieldCase,
          fieldFunctionCase,
          ifThenElseCase,
          lambdaCase,
          letDefinitionCase,
          letRecursionCase,
          listCase,
          literalCase,
          nativeApplyCase,
          patternMatchCase,
          recordCase,
          referenceCase,
          tupleCase,
          unitCase,
          updateRecordCase,
          variableCase
        )
      )
    case Lambda(attributes, argumentPattern, body) =>
      lambdaCase(
        attributes,
        argumentPattern,
        body.fold(
          applyCase,
          constructorCase,
          destructureCase,
          fieldCase,
          fieldFunctionCase,
          ifThenElseCase,
          lambdaCase,
          letDefinitionCase,
          letRecursionCase,
          listCase,
          literalCase,
          nativeApplyCase,
          patternMatchCase,
          recordCase,
          referenceCase,
          tupleCase,
          unitCase,
          updateRecordCase,
          variableCase
        )
      )
    case LetDefinition(attributes, valueName, valueDefinition, inValue) =>
      letDefinitionCase(
        attributes,
        valueName,
        valueDefinition.toCase.map(
          _.fold(
            applyCase,
            constructorCase,
            destructureCase,
            fieldCase,
            fieldFunctionCase,
            ifThenElseCase,
            lambdaCase,
            letDefinitionCase,
            letRecursionCase,
            listCase,
            literalCase,
            nativeApplyCase,
            patternMatchCase,
            recordCase,
            referenceCase,
            tupleCase,
            unitCase,
            updateRecordCase,
            variableCase
          )
        ),
        inValue.fold(
          applyCase,
          constructorCase,
          destructureCase,
          fieldCase,
          fieldFunctionCase,
          ifThenElseCase,
          lambdaCase,
          letDefinitionCase,
          letRecursionCase,
          listCase,
          literalCase,
          nativeApplyCase,
          patternMatchCase,
          recordCase,
          referenceCase,
          tupleCase,
          unitCase,
          updateRecordCase,
          variableCase
        )
      )
    case LetRecursion(attributes, valueDefinitions, inValue) =>
      letRecursionCase(
        attributes,
        valueDefinitions.map { case (n, d) =>
          (
            n,
            d.toCase.map(
              _.fold(
                applyCase,
                constructorCase,
                destructureCase,
                fieldCase,
                fieldFunctionCase,
                ifThenElseCase,
                lambdaCase,
                letDefinitionCase,
                letRecursionCase,
                listCase,
                literalCase,
                nativeApplyCase,
                patternMatchCase,
                recordCase,
                referenceCase,
                tupleCase,
                unitCase,
                updateRecordCase,
                variableCase
              )
            )
          )
        },
        inValue.fold(
          applyCase,
          constructorCase,
          destructureCase,
          fieldCase,
          fieldFunctionCase,
          ifThenElseCase,
          lambdaCase,
          letDefinitionCase,
          letRecursionCase,
          listCase,
          literalCase,
          nativeApplyCase,
          patternMatchCase,
          recordCase,
          referenceCase,
          tupleCase,
          unitCase,
          updateRecordCase,
          variableCase
        )
      )
    case ListType(attributes, elements) =>
      listCase(
        attributes,
        elements.map(
          _.fold(
            applyCase,
            constructorCase,
            destructureCase,
            fieldCase,
            fieldFunctionCase,
            ifThenElseCase,
            lambdaCase,
            letDefinitionCase,
            letRecursionCase,
            listCase,
            literalCase,
            nativeApplyCase,
            patternMatchCase,
            recordCase,
            referenceCase,
            tupleCase,
            unitCase,
            updateRecordCase,
            variableCase
          )
        )
      )
    case Literal(attributes, literal) => literalCase(attributes, literal)
    case NativeApply(attributes, function, arguments) =>
      nativeApplyCase(
        attributes,
        function,
        arguments.map(
          _.fold(
            applyCase,
            constructorCase,
            destructureCase,
            fieldCase,
            fieldFunctionCase,
            ifThenElseCase,
            lambdaCase,
            letDefinitionCase,
            letRecursionCase,
            listCase,
            literalCase,
            nativeApplyCase,
            patternMatchCase,
            recordCase,
            referenceCase,
            tupleCase,
            unitCase,
            updateRecordCase,
            variableCase
          )
        )
      )
    case PatternMatch(attributes, branchOutOn, cases) =>
      patternMatchCase(
        attributes,
        branchOutOn.fold(
          applyCase,
          constructorCase,
          destructureCase,
          fieldCase,
          fieldFunctionCase,
          ifThenElseCase,
          lambdaCase,
          letDefinitionCase,
          letRecursionCase,
          listCase,
          literalCase,
          nativeApplyCase,
          patternMatchCase,
          recordCase,
          referenceCase,
          tupleCase,
          unitCase,
          updateRecordCase,
          variableCase
        ),
        cases.map { case (p, v) =>
          (
            p,
            v.fold(
              applyCase,
              constructorCase,
              destructureCase,
              fieldCase,
              fieldFunctionCase,
              ifThenElseCase,
              lambdaCase,
              letDefinitionCase,
              letRecursionCase,
              listCase,
              literalCase,
              nativeApplyCase,
              patternMatchCase,
              recordCase,
              referenceCase,
              tupleCase,
              unitCase,
              updateRecordCase,
              variableCase
            )
          )
        }
      )
    case Record(attributes, fields) =>
      recordCase(
        attributes,
        fields.map { case (n, v) =>
          (
            n,
            v.fold(
              applyCase,
              constructorCase,
              destructureCase,
              fieldCase,
              fieldFunctionCase,
              ifThenElseCase,
              lambdaCase,
              letDefinitionCase,
              letRecursionCase,
              listCase,
              literalCase,
              nativeApplyCase,
              patternMatchCase,
              recordCase,
              referenceCase,
              tupleCase,
              unitCase,
              updateRecordCase,
              variableCase
            )
          )
        }
      )
    case Reference(attributes, name) => referenceCase(attributes, name)
    case Tuple(attributes, elements) =>
      tupleCase(
        attributes,
        elements.map(
          _.fold(
            applyCase,
            constructorCase,
            destructureCase,
            fieldCase,
            fieldFunctionCase,
            ifThenElseCase,
            lambdaCase,
            letDefinitionCase,
            letRecursionCase,
            listCase,
            literalCase,
            nativeApplyCase,
            patternMatchCase,
            recordCase,
            referenceCase,
            tupleCase,
            unitCase,
            updateRecordCase,
            variableCase
          )
        )
      )
    case UnitType(attributes) => unitCase(attributes)
    case UpdateRecord(attributes, valueToUpdate, fieldsToUpdate) =>
      updateRecordCase(
        attributes,
        valueToUpdate.fold(
          applyCase,
          constructorCase,
          destructureCase,
          fieldCase,
          fieldFunctionCase,
          ifThenElseCase,
          lambdaCase,
          letDefinitionCase,
          letRecursionCase,
          listCase,
          literalCase,
          nativeApplyCase,
          patternMatchCase,
          recordCase,
          referenceCase,
          tupleCase,
          unitCase,
          updateRecordCase,
          variableCase
        ),
        fieldsToUpdate.map { case (n, v) =>
          (
            n,
            v.fold(
              applyCase,
              constructorCase,
              destructureCase,
              fieldCase,
              fieldFunctionCase,
              ifThenElseCase,
              lambdaCase,
              letDefinitionCase,
              letRecursionCase,
              listCase,
              literalCase,
              nativeApplyCase,
              patternMatchCase,
              recordCase,
              referenceCase,
              tupleCase,
              unitCase,
              updateRecordCase,
              variableCase
            )
          )
        }
      )
    case Variable(attributes, name) => variableCase(attributes, name)
  }

  def foldLeft[Z](initial: Z)(f: (Z, Value[TA, VA]) => Z): Z = {
    @tailrec
    def loop(stack: List[Value[TA, VA]], acc: Z): Z =
      stack match {
        case Nil                                   => acc
        case (t @ Apply(_, _, _)) :: tail          => loop(t.function :: t.argument :: tail, f(acc, t))
        case (t @ Constructor(_, _)) :: tail       => loop(tail, f(acc, t))
        case (t @ Destructure(_, _, _, _)) :: tail => loop(t.valueToDestruct :: t.inValue :: tail, f(acc, t))
        case (t @ Field(_, _, _)) :: tail          => loop(t.target :: tail, f(acc, t))
        case (t @ FieldFunction(_, _)) :: tail     => loop(tail, f(acc, t))
        case (t @ IfThenElse(_, _, _, _)) :: tail =>
          loop(t.condition :: t.thenBranch :: t.elseBranch :: tail, f(acc, t))
        case (t @ Lambda(_, _, _)) :: tail           => loop(t.body :: tail, f(acc, t))
        case (t @ LetDefinition(_, _, _, _)) :: tail => loop(t.valueDefinition.body :: t.inValue :: tail, f(acc, t))
        case (t @ LetRecursion(_, _, _)) :: tail =>
          loop(t.valueDefinitions.map(_._2.body).toList ::: t.inValue :: tail, f(acc, t))
        case (t @ ListType(_, _)) :: tail        => loop(t.elements.toList ::: tail, f(acc, t))
        case (t @ Literal(_, _)) :: tail         => loop(tail, f(acc, t))
        case (t @ NativeApply(_, _, _)) :: tail  => loop(t.arguments.toList ::: tail, f(acc, t))
        case (t @ PatternMatch(_, _, _)) :: tail => loop(t.branchOutOn :: t.cases.map(_._2).toList ::: tail, f(acc, t))
        case (t @ Record(_, _)) :: tail          => loop(t.fields.map(_._2).toList ::: tail, f(acc, t))
        case (t @ Reference(_, _)) :: tail       => loop(tail, f(acc, t))
        case (t @ Tuple(_, _)) :: tail           => loop(t.elements.toList ::: tail, f(acc, t))
        case (t @ UnitType(_)) :: tail           => loop(tail, f(acc, t))
        case (t @ UpdateRecord(_, _, _)) :: tail =>
          loop(t.valueToUpdate :: t.fieldsToUpdate.map(_._2).toList ::: tail, f(acc, t))
        case (t @ Variable(_, _)) :: tail => loop(tail, f(acc, t))
      }

    loop(List(self), initial)
  }

}

object Value {

  final case class Apply[+TA, +VA](attributes: VA, function: Value[TA, VA], argument: Value[TA, VA])
      extends Value[TA, VA]

  object Apply {
    def apply[TA, VA](
        attributes: VA,
        function: Value[TA, VA],
        arg1: Value[TA, VA],
        arg2: Value[TA, VA],
        rest: Value[TA, VA]*
    ): Apply[TA, VA] =
      (arg2 +: rest).foldLeft(Apply(attributes, function, arg1)) { case (acc, arg) =>
        Apply(acc.attributes, acc, arg)
      }

    type Raw = Apply[Any, Any]

    object Raw {
      // foo: A -> B -> C
      // foo: a b
      def apply(function: RawValue, argument: RawValue, rest: RawValue*): Raw =
        rest.foldLeft(Apply((), function, argument)) { case (acc, next) => Apply((), acc, next) }

    }

    type Typed = Apply[Any, UType]
    object Typed {
      def apply(function: TypedValue, argument: TypedValue, rest: TypedValue*): Typed =
        rest.foldLeft(Apply(function.attributes, function, argument)) { case (acc, next) =>
          Apply(acc.attributes, acc, next)
        }
    }
  }

  final case class Constructor[+VA](attributes: VA, name: FQName) extends Value[Nothing, VA]
  object Constructor {
    type Raw = Constructor[Any]
    object Raw {
      def apply(name: FQName): Raw = Constructor((), name)
    }
    type Typed = Constructor[UType]
    object Typed {
      def apply(name: FQName)(ascribedType: UType): Typed   = Constructor(ascribedType, name)
      def apply(fqName: String)(ascribedType: UType): Typed = Constructor(ascribedType, FQName.fromString(fqName))
    }
  }

  final case class Destructure[+TA, +VA](
      attributes: VA,
      pattern: Pattern[VA],
      valueToDestruct: Value[TA, VA],
      inValue: Value[TA, VA]
  ) extends Value[TA, VA]

  object Destructure {
    type Raw = Destructure[Any, Any]
    object Raw {
      def apply(pattern: Pattern[Any], valueToDestruct: RawValue, inValue: RawValue): Raw =
        Destructure((), pattern, valueToDestruct, inValue)
    }

    type Typed = Destructure[Any, UType]
    object Typed {
      def apply(pattern: Pattern[UType], valueToDestruct: TypedValue, inValue: TypedValue): Typed =
        Destructure(valueToDestruct.attributes, pattern, valueToDestruct, inValue)
    }
  }

  final case class Field[+TA, +VA](attributes: VA, target: Value[TA, VA], name: Name) extends Value[TA, VA]

  object Field {
    type Raw = Field[Any, Any]
    object Raw {
      def apply(target: RawValue, name: Name): Raw   = Field((), target, name)
      def apply(target: RawValue, name: String): Raw = Field((), target, Name.fromString(name))
    }
    type Typed = Field[Any, UType]
    object Typed {
      def apply(target: TypedValue, name: Name)(ascribedType: UType): Typed = Field(ascribedType, target, name)
      def apply(fieldType: UType, target: TypedValue, name: Name): Typed    = Field(fieldType, target, name)
      def apply(target: TypedValue, name: String)(ascribedType: UType): Typed =
        Field(ascribedType, target, Name.fromString(name))
      def apply(fieldType: UType, target: TypedValue, name: String): Typed =
        Field(fieldType, target, Name.fromString(name))
    }
  }
  final case class FieldFunction[+VA](attributes: VA, name: Name) extends Value[Nothing, VA]

  object FieldFunction {
    type Raw = FieldFunction[Any]
    object Raw {
      def apply(name: Name): Raw   = FieldFunction((), name)
      def apply(name: String): Raw = FieldFunction((), Name.fromString(name))
    }
    type Typed = FieldFunction[UType]
    object Typed {
      def apply(name: Name)(ascribedType: UType): Typed   = FieldFunction(ascribedType, name)
      def apply(name: String)(ascribedType: UType): Typed = FieldFunction(ascribedType, Name.fromString(name))
    }
  }

  final case class IfThenElse[+TA, +VA](
      attributes: VA,
      condition: Value[TA, VA],
      thenBranch: Value[TA, VA],
      elseBranch: Value[TA, VA]
  ) extends Value[TA, VA]

  object IfThenElse {
    type Raw = IfThenElse[Any, Any]
    object Raw {
      def apply(condition: RawValue, thenBranch: RawValue, elseBranch: RawValue): Raw =
        IfThenElse((), condition, thenBranch, elseBranch)
    }

    type Typed = IfThenElse[Any, UType]
    object Typed {
      def apply(condition: TypedValue, thenBranch: TypedValue, elseBranch: TypedValue): Typed =
        IfThenElse(condition.attributes, condition, thenBranch, elseBranch)
    }
  }

  final case class Lambda[+TA, +VA](attributes: VA, argumentPattern: Pattern[VA], body: Value[TA, VA])
      extends Value[TA, VA]

  object Lambda {
    type Raw = Lambda[Any, Any]
    object Raw {
      def apply(argumentPattern: Pattern[Any], body: RawValue): Raw =
        Lambda((), argumentPattern, body)
    }
    type Typed = Lambda[Any, UType]
    object Typed {
      def apply(argumentPattern: Pattern[UType], body: TypedValue): Typed =
        Lambda(body.attributes, argumentPattern, body)
    }
  }

  final case class LetDefinition[+TA, +VA](
      attributes: VA,
      valueName: Name,
      valueDefinition: Definition[TA, VA],
      inValue: Value[TA, VA]
  ) extends Value[TA, VA]

  object LetDefinition {
    type Raw = LetDefinition[Any, Any]
    object Raw {
      def apply(valueName: Name, valueDefinition: Definition[Any, Any], inValue: RawValue): Raw =
        LetDefinition((), valueName, valueDefinition, inValue)

      def apply(valueName: String, valueDefinition: Definition[Any, Any], inValue: RawValue): Raw =
        LetDefinition((), Name.fromString(valueName), valueDefinition, inValue)
    }

    type Typed = LetDefinition[Any, UType]
    object Typed {
      def apply(valueName: Name, valueDefinition: Definition.Typed, inValue: TypedValue): Typed =
        LetDefinition(inValue.attributes, valueName, valueDefinition, inValue)

      def apply(valueName: String, valueDefinition: Definition.Typed, inValue: TypedValue): Typed =
        LetDefinition(inValue.attributes, Name.fromString(valueName), valueDefinition, inValue)
    }

  }

  final case class LetRecursion[+TA, +VA](
      attributes: VA,
      valueDefinitions: Map[Name, Definition[TA, VA]],
      inValue: Value[TA, VA]
  ) extends Value[TA, VA]

  object LetRecursion {
    def apply[TA, VA](attributes: VA, defs: (String, Definition[TA, VA])*)(
        inValue: Value[TA, VA]
    ): LetRecursion[TA, VA] =
      LetRecursion(attributes, defs.map { case (n, v) => (Name.fromString(n), v) }.toMap, inValue)

    type Raw = LetRecursion[Any, Any]
    object Raw {
      def apply(valueDefinitions: Map[Name, Definition[Any, Any]], inValue: RawValue): Raw =
        LetRecursion((), valueDefinitions, inValue)

      def apply(defs: (String, Definition[Any, Any])*)(inValue: RawValue): Raw =
        LetRecursion((), defs.map { case (n, v) => (Name.fromString(n), v) }.toMap, inValue)
    }
    type Typed = LetRecursion[Any, UType]
    object Typed {
      def apply(valueDefinitions: Map[Name, Definition[Any, UType]], inValue: TypedValue): Typed =
        LetRecursion(inValue.attributes, valueDefinitions, inValue)
      def apply[VA](defs: (String, Definition[Any, UType])*)(inValue: TypedValue): Typed =
        LetRecursion(inValue.attributes, defs.map { case (n, v) => (Name.fromString(n), v) }.toMap, inValue)
    }
  }

  final case class List[+TA, +VA](attributes: VA, elements: Chunk[Value[TA, VA]]) extends Value[TA, VA]

  object List {
    def nonEmpty[TA, VA](first: Value[TA, VA], rest: Value[TA, VA]*): List[TA, VA] =
      List(first.attributes, first +: Chunk.fromIterable(rest))
    type Raw = List[Any, Any]
    object Raw {
      def apply(elements: RawValue*): Raw       = List((), Chunk.fromArray(elements.toArray))
      def apply(elements: Chunk[RawValue]): Raw = List((), elements)
    }

    type Typed = List[Any, UType]
    object Typed {
      def empty(ascribedType: UType): Typed                              = List(ascribedType, Chunk.empty)
      def apply(elements: Chunk[TypedValue])(ascribedType: UType): Typed = List(ascribedType, elements)
      def apply(elements: TypedValue*)(ascribedType: UType): Typed = List(ascribedType, Chunk.fromIterable(elements))
    }
  }

  final case class Literal[+VA, +A](attributes: VA, literal: Lit[A]) extends Value[Nothing, VA]

  object Literal {
    def apply[A](literal: Lit[A])(implicit ev: InferredTypeOf[Lit[A]]): Typed[A] =
      Literal(ev.inferredType(literal), literal)
    type Raw[+A] = Literal[Any, A]
    object Raw {
      def apply[A](literal: Lit[A]): Raw[A] = Literal((), literal)
    }

    type Typed[+A] = Literal[UType, A]
    object Typed {
      def apply[A](literal: Lit[A]): Typed[A]                      = Literal(literal.inferredType, literal)
      def apply[A](literal: Lit[A])(ascribedType: UType): Typed[A] = Literal(ascribedType, literal)
    }
  }

  final case class NativeApply[+TA, +VA](attributes: VA, function: NativeFunction, arguments: Chunk[Value[TA, VA]])
      extends Value[TA, VA]

  object NativeApply {
    type Raw = NativeApply[Any, Any]
    object Raw {
      def apply(function: NativeFunction, arguments: Chunk[RawValue]): Raw =
        NativeApply((), function, arguments)
    }
    type Typed = NativeApply[Any, UType]
    object Typed {
      def apply(function: NativeFunction, arguments: Chunk[TypedValue])(returnType: UType): Typed =
        NativeApply(returnType, function, arguments)
    }
  }

  final case class PatternMatch[+TA, +VA](
      attributes: VA,
      branchOutOn: Value[TA, VA],
      cases: Chunk[(Pattern[VA], Value[TA, VA])]
  ) extends Value[TA, VA]

  object PatternMatch {
    type Raw = PatternMatch[Any, Any]
    object Raw {
      def apply(branchOutOn: RawValue, cases: Chunk[(Pattern[Any], RawValue)]): Raw =
        PatternMatch((), branchOutOn, cases)

      def apply(branchOutOn: RawValue, cases: (Pattern[Any], RawValue)*): Raw =
        PatternMatch((), branchOutOn, Chunk.fromIterable(cases))
    }

    type Typed = PatternMatch[Any, UType]
    object Typed {
      def apply(branchOutOn: TypedValue, cases: Chunk[(Pattern[UType], TypedValue)]): Typed =
        PatternMatch(branchOutOn.attributes, branchOutOn, cases)

      def apply(branchOutOn: TypedValue, cases: (Pattern[UType], TypedValue)*): Typed =
        PatternMatch(branchOutOn.attributes, branchOutOn, Chunk.fromIterable(cases))
    }
  }

  final case class Record[+TA, +VA](attributes: VA, fields: Chunk[(Name, Value[TA, VA])]) extends Value[TA, VA]

  object Record {
    import zio.morphir.ir.Type.Field.defineField
    type Raw = Record[Any, Any]

    object Raw {
      def apply(fields: Chunk[(Name, RawValue)]): Raw = Record((), fields)
      def apply(fields: (String, RawValue)*): Raw = Record(
        attributes = (),
        fields = Chunk.fromIterable(fields).map { case (n, v) => Name.fromString(n) -> v }
      )
    }

    type Typed = Record[Any, UType]
    object Typed {
      def apply(recordType: UType, fields: (String, TypedValue)*): Typed = Record(
        attributes = recordType,
        fields = Chunk.fromIterable(fields).map { case (n, v) => (Name.fromString(n), v) }
      )

      def apply(fields: (String, TypedValue)*): Typed = {
        val allFields  = Chunk.fromIterable(fields.map { case (n, v) => (Name.fromString(n), v) })
        val recordType = Type.record(allFields.map { case (n, v) => defineField(n, v.attributes) })
        Record(recordType, allFields)
      }
    }
  }

  final case class Reference[+VA](attributes: VA, name: FQName) extends Value[Nothing, VA]

  object Reference {
    def apply(name: FQName): Raw = Reference((), name)

    type Raw = Reference[Any]
    object Raw {
      def apply(name: FQName): Raw = Reference((), name)
      def apply(name: String): Raw = Reference((), FQName.fromString(name))
    }

    type Typed = Reference[UType]
    object Typed {
      def apply(fqName: String)(refType: UType): Typed = Reference(refType, FQName.fromString(fqName))
      def apply(name: FQName)(refType: UType): Typed   = Reference(refType, name)
    }
  }

  final case class Tuple[+TA, +VA](attributes: VA, elements: Chunk[Value[TA, VA]]) extends Value[TA, VA]

  object Tuple {
    def apply(elements: (RawValue, UType)*): Typed =
      Tuple(
        Type.tuple(Chunk.fromIterable(elements.map(_._2))),
        Chunk(elements: _*).map { case (v, t) => v :@ t }
      )

    val empty: Raw = Tuple((), Chunk.empty)
    type Raw = Tuple[Any, Any]

    object Raw {
      def apply(elements: RawValue*): Raw       = Tuple((), Chunk(elements: _*))
      def apply(elements: Chunk[RawValue]): Raw = Tuple((), elements)
    }

    type Typed = Tuple[Any, UType]
    object Typed {

      def apply(elements: Chunk[TypedValue]): Typed = {
        val tupleTypes: Chunk[UType] = elements.map(_.attributes)
        val tupleType                = Type.tuple(tupleTypes)
        Tuple(attributes = tupleType, elements = elements)
      }
      def apply(elements: TypedValue*): Typed = {
        val tupleTypes: Chunk[UType] = Chunk.fromIterable(elements.map(_.attributes))
        val tupleType                = Type.tuple(tupleTypes)
        Tuple(tupleType, Chunk(elements: _*))
      }
    }
  }

  final case class Unit[+VA](attributes: VA) extends Value[Nothing, VA]
  object Unit {
    type Raw = Unit[Any]
    def Raw: Raw = Unit(())

    type Typed = Unit[UType]
    object Typed {
      def apply: Typed = Value.Unit(Type.unit)
    }
  }

  final case class UpdateRecord[+TA, +VA](
      attributes: VA,
      valueToUpdate: Value[TA, VA],
      fieldsToUpdate: Chunk[(Name, Value[TA, VA])]
  ) extends Value[TA, VA]

  object UpdateRecord {
    type Raw = UpdateRecord[Any, Any]
    object Raw {
      def apply(valueToUpdate: RawValue, fieldsToUpdate: Chunk[(Name, RawValue)]): Raw =
        UpdateRecord((), valueToUpdate, fieldsToUpdate)

      def apply(valueToUpdate: RawValue, fieldsToUpdate: (String, RawValue)*): Raw =
        UpdateRecord(
          (),
          valueToUpdate,
          Chunk.fromIterable(fieldsToUpdate).map { case (n, v) => Name.fromString(n) -> v }
        )
    }

    type Typed = UpdateRecord[Any, UType]
    object Typed {
      def apply(valueToUpdate: TypedValue, fieldsToUpdate: Chunk[(Name, TypedValue)]): Typed =
        UpdateRecord(
          valueToUpdate.attributes,
          valueToUpdate,
          fieldsToUpdate
        )

      def apply(valueToUpdate: TypedValue, fieldsToUpdate: (String, TypedValue)*): Typed =
        UpdateRecord(
          valueToUpdate.attributes,
          valueToUpdate,
          Chunk.fromIterable(fieldsToUpdate).map { case (n, v) => (Name.fromString(n), v) }
        )
    }
  }

  final case class Variable[+VA](attributes: VA, name: Name) extends Value[Nothing, VA]
  object Variable {
    def apply[VA](attributes: VA, name: String): Variable[VA] = Variable(attributes, Name.fromString(name))

    type Raw = Variable[Any]
    object Raw {
      def apply(name: Name): Raw   = Variable((), name)
      def apply(name: String): Raw = Variable((), Name.fromString(name))
    }
    type Typed = Variable[UType]
    object Typed {
      def apply(name: Name, variableType: UType): Typed   = Variable(variableType, name)
      def apply(name: String, variableType: UType): Typed = Variable(variableType, Name.fromString(name))
    }
  }

  implicit class RawValueExtensions(private val self: RawValue) extends AnyVal {

    /**
     * Ascribe the given type to this `RawValue` and all its children.
     * ===NOTE===
     * This is a recursive operation and all children of this `RawValue` will also be ascribed with the given value.
     */
    def :@(ascribedType: UType): TypedValue = self.mapAttributes(identity, _ => ascribedType)

    /**
     * Ascribe the given type to this `RawValue` and all its children.
     * ===NOTE===
     * This is a recursive operation and all children of this `RawValue` will also be ascribed with the given value.
     */
    def @:(ascribedType: UType): TypedValue = self.mapAttributes(identity, _ => ascribedType)

    def toDefinition(returnType: UType): Definition.Raw = Definition.fromRawValue(self, returnType)
  }

  implicit class TypedValueExtensions(private val self: TypedValue) extends AnyVal {

    /**
     * Ascribe the given type to the value.
     * ===NOTE===
     * This is not a recursive operation on a `TypedValue` and it will only ascribe the type of this given value and not
     * its children.
     */
    def :@(ascribedType: UType): TypedValue = self match {
      case Apply(_, function, arguments) => Apply(ascribedType, function, arguments)
      case Constructor(_, name)          => Constructor(ascribedType, name)
      case Destructure(_, pattern, valueToDestruct, inValue) =>
        Destructure(ascribedType, pattern, valueToDestruct, inValue)
      case Field(_, target, name) => Field(ascribedType, target, name)
      case FieldFunction(_, name) => FieldFunction(ascribedType, name)
      case IfThenElse(_, condition, thenBranch, elseBranch) =>
        IfThenElse(ascribedType, condition, thenBranch, elseBranch)
      case Lambda(_, argumentPattern, body) => Lambda(ascribedType, argumentPattern, body)
      case LetDefinition(_, valueName, valueDefinition, inValue) =>
        LetDefinition(ascribedType, valueName, valueDefinition, inValue)
      case LetRecursion(_, valueDefinitions, inValue)     => LetRecursion(ascribedType, valueDefinitions, inValue)
      case List(_, elements)                              => List(ascribedType, elements)
      case Literal(_, literal)                            => Literal(ascribedType, literal)
      case NativeApply(_, function, arguments)            => NativeApply(ascribedType, function, arguments)
      case PatternMatch(_, branchOutOn, cases)            => PatternMatch(ascribedType, branchOutOn, cases)
      case Record(_, fields)                              => Record(ascribedType, fields)
      case Reference(_, name)                             => Reference(ascribedType, name)
      case Tuple(_, elements)                             => Tuple(ascribedType, elements)
      case Unit(_)                                        => Unit(ascribedType)
      case UpdateRecord(_, valueToUpdate, fieldsToUpdate) => UpdateRecord(ascribedType, valueToUpdate, fieldsToUpdate)
      case Variable(_, name)                              => Variable(ascribedType, name)
    }

    def toDefinition: Definition.Typed = Definition.fromTypedValue(self)
  }
}
