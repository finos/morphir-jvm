package zio.morphir.ir.value

import zio.Chunk
import zio.morphir.ir.{Literal => Lit, _}
import zio.morphir.ir.types.UType
import zio.morphir.ir.value.Value.{Unit => UnitType, _}
trait ValueSyntax {

  def apply(fqName: FQName, argument: TypedValue, arguments: TypedValue*)(returnType: UType): TypedValue =
    Apply.Typed(Reference.Typed(fqName)(returnType), argument, arguments: _*)

  def apply(function: RawValue, argument: RawValue, arguments: RawValue*): RawValue =
    Apply.Raw(function, argument, arguments: _*)

  def applyStrict(function: TypedValue, argument: TypedValue, arguments: TypedValue*): TypedValue =
    Apply.Typed(function, argument, arguments: _*)

  final def boolean[Attributes](value: Boolean, attributes: Attributes): Value[Nothing, Attributes] =
    Literal(attributes, Lit.boolean(value))

  final def caseOf[TA, VA](value: Value[TA, VA])(cases: (Pattern[VA], Value[TA, VA])*): Value[TA, VA] =
    PatternMatch(value.attributes, value, Chunk.fromIterable(cases))

  def constructor(name: FQName): RawValue = Constructor.Raw(name)
  def constructor[VA](attributes: VA, name: FQName): Value[Nothing, VA] =
    Constructor(attributes, name)

  final def boolean(value: Boolean): RawValue = literal(Lit.boolean(value))

  def definition(args: (String, UType)*)(returnType: UType)(body: TypedValue): Definition.Typed =
    Definition.Typed(args: _*)(returnType)(body)

  def destructure(pattern: UPattern, valueToDestruct: RawValue, inValue: RawValue): RawValue =
    Destructure.Raw(pattern, valueToDestruct, inValue)

  def field(target: RawValue, name: Name): RawValue         = Field.Raw(target, name)
  final def field(target: RawValue, name: String): RawValue = Field.Raw(target, Name.fromString(name))

  def fieldFunction(name: Name): RawValue                    = FieldFunction.Raw(name)
  def fieldFunction(name: String): RawValue                  = FieldFunction.Raw(name)
  def fieldFunction(name: String, `type`: UType): TypedValue = FieldFunction.Typed(name)(`type`)

  def ifThenElse(condition: RawValue, thenBranch: RawValue, elseBranch: RawValue): RawValue =
    IfThenElse.Raw(condition, thenBranch, elseBranch)

  final def int(value: Int): RawValue = literal(Lit.int(value))

  final def lambda(pattern: UPattern, body: RawValue): RawValue =
    Lambda.Raw(pattern, body)

  def letDefinition(valueName: Name, valueDefinition: UDefinition, inValue: RawValue): RawValue =
    LetDefinition.Raw(valueName, valueDefinition, inValue)

  def letDefinition(name: String, definition: Definition.Typed, body: TypedValue): TypedValue =
    LetDefinition.Typed(name, definition, body)

  def letRecursion(valueDefinitions: Map[Name, UDefinition], inValue: RawValue): RawValue =
    LetRecursion.Raw(valueDefinitions, inValue)

  def list(elements: Chunk[RawValue]): RawValue                     = List.Raw(elements)
  def list(elements: RawValue*): RawValue                           = List.Raw(Chunk.fromIterable(elements))
  def listOf(elementType: UType)(elements: TypedValue*): TypedValue = List.Typed(elements: _*)(elementType)

  def literal[T](literal: Lit[T]): RawValue = Literal.Raw(literal)
  def literal(int: Int): RawValue           = Literal.Raw(Lit.int(int))
  def literal(string: String): TypedValue   = Lit.string(string).toTypedValue
  def literal(boolean: Boolean): RawValue   = Literal.Raw(Lit.boolean(boolean))

  final def literal[VA, V](value: Lit[V])(attributes: VA): Value[Nothing, VA] =
    Literal(attributes, value)

  def patternMatch(scrutinee: RawValue, cases: (UPattern, RawValue)*): RawValue =
    PatternMatch.Raw(scrutinee, Chunk.fromIterable(cases))

  def patternMatch(scrutinee: RawValue, cases: Chunk[(UPattern, RawValue)]): RawValue =
    PatternMatch.Raw(scrutinee, cases)

  def record(fields: (Name, RawValue)*): RawValue =
    Record.Raw(Chunk.fromIterable(fields))

  def record(firstField: (String, RawValue), restFields: (String, RawValue)*): RawValue =
    Record.Raw((firstField +: restFields): _*)

  def record(fields: Chunk[(Name, RawValue)]): RawValue =
    Record.Raw(fields)

  def reference[VA](name: FQName, attributes: VA): Value[Nothing, VA] = Reference(attributes, name)
  def reference(name: FQName): RawValue                               = Reference(name)
  def reference(name: FQName, tpe: UType): TypedValue                 = Reference(tpe, name)
  def reference(name: String): RawValue                               = Reference.Raw(name)

  final def string(value: String): RawValue = literal(Lit.string(value))
  final def string[Attributes](value: String, attributes: Attributes): Literal[Attributes, String] =
    Literal(attributes, Lit.string(value))

  def tuple(elements: Chunk[RawValue]): Tuple.Raw    = Tuple.Raw(elements)
  def tuple(elements: RawValue*): Tuple.Raw          = Tuple.Raw(Chunk.fromIterable(elements))
  def tupleTyped(elements: TypedValue*): Tuple.Typed = Tuple.Typed(elements: _*)

  final val unit: RawValue                                                       = UnitType.Raw
  final def unit[Attributes](attributes: Attributes): Value[Nothing, Attributes] = UnitType(attributes)

  def updateRecord(valueToUpdate: RawValue, fieldsToUpdate: Chunk[(Name, RawValue)]): RawValue =
    UpdateRecord.Raw(valueToUpdate, fieldsToUpdate)

  final def variable(name: Name): RawValue                           = Variable.Raw(name)
  final def variable[A](name: Name, variableType: UType): TypedValue = Variable(variableType, name)

  @inline final def variable(string: String): RawValue = variable(Name.fromString(string))

  @inline final def variable[A](string: String, variableType: UType): TypedValue =
    variable(Name.fromString(string), variableType)

  def wholeNumber(value: java.math.BigInteger): RawValue =
    literal(Lit.wholeNumber(value))

  @inline final val wildcardPattern: UPattern = Pattern.wildcardPattern
  @inline final def wildcardPattern[Attributes](
      attributes: Attributes
  ): Pattern[Attributes] =
    Pattern.wildcardPattern(attributes)

  def asPattern(pattern: UPattern, name: Name): UPattern =
    Pattern.AsPattern(pattern, name, ())

  def constructorPattern(name: FQName, patterns: Chunk[UPattern]): UPattern =
    Pattern.ConstructorPattern(name, patterns, ())

  def emptyListPattern: UPattern = Pattern.EmptyListPattern(())

  def headTailPattern(head: UPattern, tail: UPattern): UPattern =
    Pattern.HeadTailPattern(head, tail, ())

  def literalPattern[A](literal: Lit[A]): Pattern.LiteralPattern[A, Unit] =
    Pattern.LiteralPattern(literal, ())

  def literalPattern(value: String): Pattern.LiteralPattern[String, Unit] =
    Pattern.LiteralPattern(Lit.string(value), ())

  def literalPattern(int: Int): Pattern.LiteralPattern[java.math.BigInteger, Unit] =
    Pattern.LiteralPattern(Lit.int(int), ())

  def literalPattern(boolean: Boolean): Pattern.LiteralPattern[Boolean, Unit] =
    Pattern.LiteralPattern(Lit.boolean(boolean), ())

  def tuplePattern(patterns: UPattern*): UPattern =
    Pattern.TuplePattern(Chunk.fromIterable(patterns), ())

  def nativeApply(function: NativeFunction, arguments: Chunk[RawValue]): RawValue =
    NativeApply((), function, arguments)

  def nativeApply(function: NativeFunction, arguments: RawValue*): RawValue =
    NativeApply((), function, Chunk.fromIterable(arguments))

  val unitPattern: UPattern = Pattern.UnitPattern(())

}

object ValueSyntax extends ValueSyntax {}
