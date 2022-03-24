package zio.morphir.syntax

import zio.Chunk
import zio.morphir.ir.{Literal => Lit, _}
import ValueModule.{RawValue, Value, ValueCase, ValueDefinition}
import zio.morphir.ir.TypeModule.Type
import zio.morphir.ir.ValueModule.ValueCase._

trait ValueSyntax {

  import ValueCase._

  def apply(function: Value[Any], arguments: Chunk[Value[Any]]): Value[Any] = Value(ApplyCase(function, arguments))
  def apply(function: Value[Any], arguments: Value[Any]*): Value[Any] = Value(
    ApplyCase(function, Chunk.fromIterable(arguments))
  )

  def applyStrict(function: Value[UType], arguments: Chunk[Value[UType]]): Value[UType] =
    Value(ApplyCase(function, arguments), function.annotations)
  def applyStrict(function: Value[UType], arguments: Value[UType]*): Value[UType] =
    Value(ApplyCase(function, Chunk.fromIterable(arguments)), function.annotations)

  final def boolean[Attributes](
      value: Boolean,
      attributes: Attributes
  ): Value[Any] = literal(Lit.boolean(value), attributes)

  def constructor(name: FQName): Value[Any] = Value(ConstructorCase(name))
  def constructor[Attributes](name: FQName, attributes: Attributes): Value[Attributes] =
    Value(ConstructorCase(name), attributes)

  final def boolean(value: Boolean): Value[Any] = literal(Lit.boolean(value))

  def destructure(pattern: Pattern[Any], valueToDestruct: Value[Any], inValue: Value[Any]): Value[Any] =
    Value(DestructureCase(pattern, valueToDestruct, inValue))

  def field(tag: Value[Any], name: Name): Value[Any]         = Value(FieldCase(tag, name))
  final def field(tag: Value[Any], name: String): Value[Any] = Value(FieldCase(tag, Name.fromString(name)))

  def fieldFunction(name: Name): Value[Any] = Value(FieldFunctionCase(name))

  def ifThenElse(condition: Value[Any], thenBranch: Value[Any], elseBranch: Value[Any]): Value[Any] =
    Value(IfThenElseCase(condition, thenBranch, elseBranch))

  final def int(value: Int): Value[Any] = literal(Lit.int(value))

  final def lambda(pattern: Pattern[Any], body: Value[Any]): Value[Any] =
    Value(LambdaCase(pattern, body))

  def letDefinition(valueName: Name, valueDefinition: ValueDefinition[Any], inValue: Value[Any]): Value[Any] =
    Value(LetDefinitionCase(valueName, valueDefinition, inValue))

  def letRecursion(valueDefinitions: Map[Name, ValueDefinition[Any]], inValue: Value[Any]): Value[Any] =
    Value(LetRecursionCase(valueDefinitions, inValue))

  def list(elements: Chunk[Value[Any]]): Value[Any] = Value(ListCase(elements))
  def list(elements: Value[Any]*): Value[Any]       = Value(ListCase(Chunk.fromIterable(elements)))

  def literal(literal: LiteralValue): Value[Any] = Value(LiteralCase(literal))
  def literal(int: Int): Value[Any]              = Value(LiteralCase(Lit.int(int)))
  def literal(string: String): Value[Any]        = Value(LiteralCase(Lit.string(string)))
  def literal(boolean: Boolean): Value[Any]      = Value(LiteralCase(Lit.boolean(boolean)))

  final def literal[V, Attributes](value: Lit[V], attributes: Attributes): Value[Any] =
    Value(LiteralCase(value), attributes)

  def patternMatch(scrutinee: Value[Any], cases: (Pattern[Any], Value[Any])*): Value[Any] =
    Value(PatternMatchCase(scrutinee, Chunk.fromIterable(cases)))

  def patternMatch(scrutinee: Value[Any], cases: Chunk[(Pattern[Any], Value[Any])]): Value[Any] =
    Value(PatternMatchCase(scrutinee, cases))

  def record(fields: (Name, Value[Any])*): Value[Any] =
    Value(RecordCase(Chunk.fromIterable(fields)))

  def record(fields: Chunk[(Name, Value[Any])]): Value[Any] =
    Value(RecordCase(fields))

  def reference[Attributes](name: FQName, attributes: Attributes): Value[Attributes] =
    Value(ReferenceCase(name), attributes)
  def reference(name: FQName): Value[Any]               = Value(ReferenceCase(name))
  def reference(name: FQName, tpe: UType): Value[UType] = Value(ReferenceCase(name), tpe)

  final def string(value: String): Value[Any] = literal(Lit.string(value))
  final def string[Attributes](value: String, attributes: Attributes): Value[Any] =
    literal[String, Attributes](Lit.string(value), attributes)

  def tuple[Any](elements: Chunk[Value[Any]])  = Value(TupleCase(elements))
  def tuple(elements: Value[Any]*): Value[Any] = Value(TupleCase(Chunk.fromIterable(elements)))

  final val unit: Value[Any]                                            = Value(UnitCase)
  final def unit[Attributes](attributes: Attributes): Value[Attributes] = Value(UnitCase, attributes)

  def updateRecord(valueToUpdate: Value[Any], fieldsToUpdate: Chunk[(Name, Value[Any])]): Value[Any] =
    Value(UpdateRecordCase(valueToUpdate, fieldsToUpdate))

  final def variable(name: Name): Value[Any]                               = Value(VariableCase(name))
  final def variable[A](name: Name, variableType: Type[A]): Value[Type[A]] = Value(VariableCase(name), variableType)
  @inline final def variable(string: String): Value[Any]                   = variable(Name.fromString(string))
  @inline final def variable[A](string: String, variableType: Type[A]): Value[Type[A]] =
    variable(Name.fromString(string), variableType)

  def wholeNumber(value: java.math.BigInteger): Value[Any] =
    literal(Lit.wholeNumber(value))

  @inline final val wildcardPattern: Pattern.WildcardPattern[Any] = Pattern.wildcardPattern
  @inline final def wildcardPattern[Attributes](
      attributes: Attributes
  ): Pattern.WildcardPattern[Attributes] =
    Pattern.wildcardPattern(attributes)

  def asPattern(pattern: Pattern[Any], name: Name): Pattern.AsPattern[Any] =
    Pattern.AsPattern(pattern, name, ())

  def constructorPattern(name: FQName, patterns: Chunk[Pattern[Any]]): Pattern[Any] =
    Pattern.ConstructorPattern(name, patterns, ())

  def emptyListPattern: Pattern[Any] = Pattern.EmptyListPattern(())

  def headTailPattern(head: Pattern[Any], tail: Pattern[Any]): Pattern[Any] =
    Pattern.HeadTailPattern(head, tail, ())

  def literalPattern[A](literal: Lit[A]): Pattern.LiteralPattern[A, Any] =
    Pattern.LiteralPattern(literal, ())

  def literalPattern(value: String): Pattern.LiteralPattern[String, Any] =
    Pattern.LiteralPattern(Lit.string(value), ())

  def literalPattern(int: Int): Pattern.LiteralPattern[java.math.BigInteger, Any] =
    Pattern.LiteralPattern(Lit.int(int), ())

  def literalPattern(boolean: Boolean): Pattern.LiteralPattern[Boolean, Any] =
    Pattern.LiteralPattern(Lit.boolean(boolean), ())

  def tuplePattern(patterns: Pattern[Any]*): Pattern[Any] =
    Pattern.TuplePattern(Chunk.fromIterable(patterns), ())

  def nativeApply(function: NativeFunction, arguments: Chunk[Value[Any]]): Value[Any] =
    Value(NativeApplyCase(function, arguments), ())

  def nativeApply(function: NativeFunction, arguments: Value[Any]*): Value[Any] =
    Value(NativeApplyCase(function, Chunk.fromIterable(arguments)), ())

  val unitPattern: Pattern[Any] = Pattern.UnitPattern(())

}

object ValueSyntax {
  final def apply(function: RawValue, args: RawValue*): Value[Any] =
    Value(ApplyCase(function, Chunk.fromIterable(args)))
}
