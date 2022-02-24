package zio.morphir.syntax

import zio.{Chunk, ZEnvironment}
import zio.morphir.ir.{Literal => Lit, FQName, LiteralValue, Name, NativeFunction, ValueModule}
import ValueModule.{RawValue, Value, ValueCase, Pattern}

trait ValueSyntax {
  import Value.*
  import ValueCase.*

  final def apply(function: RawValue, args: RawValue*): Apply[Any] =
    Apply(function, Chunk.fromIterable(args), ZEnvironment.empty)

  final def boolean(value: Boolean): Literal[Boolean, Any] = Literal(Lit.boolean(value), ZEnvironment.empty)
  final def boolean[Annotations](
      value: Boolean,
      annotations: ZEnvironment[Annotations]
  ): Literal[Boolean, Annotations] =
    Literal(Lit.boolean(value), annotations)

  final def field(name: Name, record: Record[Any]): Field[Any] = Field(record, name, ZEnvironment.empty)
  final def field(name: String, record: Record[Any]): Field[Any] =
    Field(record, Name.fromString(name), ZEnvironment.empty)

  final def lambda(pattern: ValueModule.Pattern[Any], body: Value[Any]): Lambda[Any] =
    Lambda(pattern, body, ZEnvironment.empty)

  final def literal[V](value: Lit[V]): Literal[V, Any] = Literal(value, ZEnvironment.empty)
  final def literal[V, Annotations](value: Lit[V], annotations: ZEnvironment[Annotations]): Literal[V, Annotations] =
    Literal(value, annotations)

  def patternMatch(scrutinee: Value[Any], cases: (Value[Any], Value[Any])*): PatternMatch[Any] =
    PatternMatch(scrutinee, Chunk.fromIterable(cases), ZEnvironment.empty)
  def record(fields: (Name, Value[Any])*): Record[Any] = Record(Chunk.fromIterable(fields), ZEnvironment.empty)

  final def string(value: String): Literal[String, Any] = Literal(Lit.string(value), ZEnvironment.empty)
  final def string[Annotations](value: String, annotations: ZEnvironment[Annotations]): Value[Annotations] =
    Literal(Lit.string(value), annotations)

  final val unit: Unit[Any]                                                              = Unit(ZEnvironment.empty)
  final def unit[Annotations](annotations: ZEnvironment[Annotations]): Unit[Annotations] = Unit(annotations)

  final def variable(name: Name): Variable[Any]             = Variable(name, ZEnvironment.empty)
  @inline final def variable(string: String): Variable[Any] = variable(Name.fromString(string))

  def wholeNumber(value: java.math.BigInteger): Value.Literal[java.math.BigInteger, Any] =
    Value.Literal(Lit.wholeNumber(value), ZEnvironment.empty)

  val wildcardPattern: Pattern.Wildcard[Any] = Pattern.Wildcard(ZEnvironment.empty)
  def wildcardPattern[Annotations](annotations: ZEnvironment[Annotations]): Pattern.Wildcard[Annotations] =
    Pattern.Wildcard(annotations)

  def asPattern(pattern: Pattern[Any], name: Name): Pattern.AsPattern[Any] =
    Pattern.AsPattern(pattern, name, ZEnvironment.empty)

  def constructorPattern(name: FQName, patterns: Chunk[Pattern[Any]]): Value[Any] =
    Value(PatternCase.ConstructorPatternCase(name, patterns))

  def emptyListPattern: Value[Any] =
    Value(PatternCase.EmptyListPatternCase)

  def headTailPattern(head: Value[Any], tail: Value[Any]): Value[Any] =
    Value(PatternCase.HeadTailPatternCase(head, tail))

  def literalPattern(literal: Lit[Any]): Pattern.LiteralPattern[Any] =
    Pattern.LiteralPattern(literal, ZEnvironment.empty)

  def literalPattern(value: String): Pattern.LiteralPattern[Any] =
    Pattern.LiteralPattern(Lit.string(value), ZEnvironment.empty)

  def literalPattern(int: Int): Pattern.LiteralPattern[Any] =
    Pattern.LiteralPattern(Lit.int(int), ZEnvironment.empty)

  def literalPattern(boolean: Boolean): Pattern.LiteralPattern[Any] =
    Pattern.LiteralPattern(Lit.boolean(boolean), ZEnvironment.empty)

  def tuplePattern(patterns: Value[Any]*): Value[Any] =
    Value(PatternCase.TuplePatternCase(Chunk.fromIterable(patterns)))

  def nativeApply(function: NativeFunction, arguments: Chunk[Value[Any]]): Value[Any] =
    Value(NativeApplyCase(function, arguments))

  def apply(function: Value[Any], arguments: Chunk[Value[Any]]): Value[Any] =
    Value(ApplyCase(function, arguments))

  def constructor(name: FQName): Value[Any] =
    Value(ConstructorCase(name))

  def fieldCase(tag: Value[Any], name: Name): Value[Any] =
    Value(FieldCase(tag, name))

  def fieldFunction(name: Name): Value[Any] =
    Value(FieldFunctionCase(name))

  def ifThenElse(condition: Value[Any], thenBranch: Value[Any], elseBranch: Value[Any]): Value[Any] =
    Value(IfThenElseCase(condition, thenBranch, elseBranch))

  def letRecursion(valueDefinitions: Map[Name, Value[Any]], inValue: Value[Any]): Value[Any] =
    Value(LetRecursionCase(valueDefinitions, inValue))

  def list(elements: Chunk[Value[Any]]): Value[Any] =
    Value(ListCase(elements))

  def literal(literal: LiteralValue): Value[Any] =
    Value(LiteralCase(literal))

  def literal(int: Int): Value[Any] =
    Value(LiteralCase(Lit.int(int)))

  def literal(string: String): Value[Any] =
    Value(LiteralCase(Lit.string(string)))

  def literal(boolean: Boolean): Value[Any] =
    Value(LiteralCase(Lit.boolean(boolean)))

  def patternMatch(branchOutOn: Value[Any], cases: Chunk[(Pattern[Any], Value[Any])]): Value[Any] =
    Value(PatternMatchCase(branchOutOn, cases))

  def record(fields: Chunk[(Name, Value[Any])]): Value[Any] =
    Value(RecordCase(fields))

  def reference(name: FQName): Value[Any] =
    Value(ReferenceCase(name))

  val unitPattern: Value[Any] =
    Value(PatternCase.UnitPatternCase)

  def tuple(elements: Value[Any]*): Value[Any] =
    Value(TupleCase(Chunk.fromIterable(elements)))

  def letDefinition(valueName: Name, valueDefinition: Value[Any], inValue: Value[Any]): Value[Any] =
    Value(LetDefinitionCase(valueName, valueDefinition, inValue))

  def updateRecord(valueToUpdate: Value[Any], fieldsToUpdate: Chunk[(Name, Value[Any])]): Value[Any] =
    Value(UpdateRecordCase(valueToUpdate, fieldsToUpdate))

  def destructure(pattern: Value[Any], valueToDestruct: Value[Any], inValue: Value[Any]): Value[Any] =
    Value(DestructureCase(pattern, valueToDestruct, inValue))

}
