package zio.morphir.syntax

import zio.{Chunk, ZEnvironment}
import zio.morphir.ir.{Literal => Lit, _}
import ValueModule.{RawValue, Value, ValueDefinition, ValueCase}
import zio.morphir.ir.ValueModule.ValueCase.*

trait ValueSyntax {

  import ValueCase.*

  def apply(function: Value[Any], arguments: Chunk[Value[Any]]): Value[Any] = Value(ApplyCase(function, arguments))
  def apply(function: Value[Any], arguments: Value[Any]*): Value[Any] = Value(
    ApplyCase(function, Chunk.fromIterable(arguments))
  )

  final def boolean[Annotations](
      value: Boolean,
      annotations: ZEnvironment[Annotations]
  ): Value[Any] = literal(Lit.boolean(value), annotations)

  def constructor(name: FQName): Value[Any] = Value(ConstructorCase(name))

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

  def literal(literal: LiteralValue): Value[Any] = Value(LiteralCase(literal))
  def literal(int: Int): Value[Any]              = Value(LiteralCase(Lit.int(int)))
  def literal(string: String): Value[Any]        = Value(LiteralCase(Lit.string(string)))
  def literal(boolean: Boolean): Value[Any]      = Value(LiteralCase(Lit.boolean(boolean)))

  final def literal[V, Annotations](value: Lit[V], annotations: ZEnvironment[Annotations]): Value[Any] =
    Value(LiteralCase(value), annotations)

  def patternMatch(scrutinee: Value[Any], cases: (Pattern[Any], Value[Any])*): Value[Any] =
    Value(PatternMatchCase(scrutinee, Chunk.fromIterable(cases)))

  def patternMatch(scrutinee: Value[Any], cases: Chunk[(Pattern[Any], Value[Any])]): Value[Any] =
    Value(PatternMatchCase(scrutinee, cases))

  def record(fields: (Name, Value[Any])*): Value[Any] =
    Value(RecordCase(Chunk.fromIterable(fields)))

  def record(fields: Chunk[(Name, Value[Any])]): Value[Any] =
    Value(RecordCase(fields))

  def reference(name: FQName): Value[Any] = Value(ReferenceCase(name))

  final def string(value: String): Value[Any] = literal(Lit.string(value))
  final def string[Annotations](value: String, annotations: ZEnvironment[Annotations]): Value[Any] =
    literal[String, Annotations](Lit.string(value), annotations)

  def tuple[Any](elements: Chunk[Value[Any]])  = Value(TupleCase(elements))
  def tuple(elements: Value[Any]*): Value[Any] = Value(TupleCase(Chunk.fromIterable(elements)))

  final val unit: Value[Any]                                                              = Value(UnitCase)
  final def unit[Annotations](annotations: ZEnvironment[Annotations]): Value[Annotations] = Value(UnitCase, annotations)

  def updateRecord(valueToUpdate: Value[Any], fieldsToUpdate: Chunk[(Name, Value[Any])]): Value[Any] =
    Value(UpdateRecordCase(valueToUpdate, fieldsToUpdate))

  final def variable(name: Name): Value[Any]             = Value(VariableCase(name))
  @inline final def variable(string: String): Value[Any] = variable(Name.fromString(string))

  def wholeNumber(value: java.math.BigInteger): Value[Any] =
    literal(Lit.wholeNumber(value))

  @inline final val wildcardPattern: Pattern.WildcardPattern[Any] = Pattern.wildcardPattern
  @inline final def wildcardPattern[Annotations](
      annotations: ZEnvironment[Annotations]
  ): Pattern.WildcardPattern[Annotations] =
    Pattern.wildcardPattern(annotations)

  def asPattern(pattern: Pattern[Any], name: Name): Pattern.AsPattern[Any] =
    Pattern.AsPattern(pattern, name, ZEnvironment.empty)

  def constructorPattern(name: FQName, patterns: Chunk[Pattern[Any]]): Pattern[Any] =
    Pattern.ConstructorPattern(name, patterns, ZEnvironment.empty)

  def emptyListPattern: Pattern[Any] = Pattern.EmptyListPattern(ZEnvironment.empty)

  def headTailPattern(head: Pattern[Any], tail: Pattern[Any]): Pattern[Any] =
    Pattern.HeadTailPattern(head, tail, ZEnvironment.empty)

  def literalPattern[A](literal: Lit[A]): Pattern.LiteralPattern[A, Any] =
    Pattern.LiteralPattern(literal, ZEnvironment.empty)

  def literalPattern(value: String): Pattern.LiteralPattern[String, Any] =
    Pattern.LiteralPattern(Lit.string(value), ZEnvironment.empty)

  def literalPattern(int: Int): Pattern.LiteralPattern[java.math.BigInteger, Any] =
    Pattern.LiteralPattern(Lit.int(int), ZEnvironment.empty)

  def literalPattern(boolean: Boolean): Pattern.LiteralPattern[Boolean, Any] =
    Pattern.LiteralPattern(Lit.boolean(boolean), ZEnvironment.empty)

  def tuplePattern(patterns: Pattern[Any]*): Pattern[Any] =
    Pattern.TuplePattern(Chunk.fromIterable(patterns), ZEnvironment.empty)

  def nativeApply(function: NativeFunction, arguments: Chunk[Value[Any]]): Value[Any] =
    Value(NativeApplyCase(function, arguments), ZEnvironment.empty)

  val unitPattern: Pattern[Any] = Pattern.UnitPattern(ZEnvironment.empty)

}

object ValueSyntax {
  final def apply(function: RawValue, args: RawValue*): Value[Any] =
    Value(ApplyCase(function, Chunk.fromIterable(args)))
}
