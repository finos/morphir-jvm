package zio.morphir.syntax

import zio.{Chunk, ZEnvironment}
import zio.morphir.ir.{Literal => Lit, Name, ValueModule}
import ValueModule.{RawValue, Value}

trait ValueSyntax {
  import Value.*

  def apply(function: RawValue, args: RawValue*): Apply[Any] =
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

  final def lambda(pattern: Value.Pattern[Any], body: Value[Any]): Lambda[Any] =
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

  final def variable(name: Name): Variable[Any] = Variable(name, ZEnvironment.empty)

  def wholeNumber(value: java.math.BigInteger): Value.Literal[java.math.BigInteger, Any] = {
    println("In Value.wholeNumber")
    Value.Literal(Lit.wholeNumber(value), ZEnvironment.empty)
  }

  val wildcard: Pattern.Wildcard[Any] = Pattern.Wildcard(ZEnvironment.empty)
  def wildcard[Annotations](annotations: ZEnvironment[Annotations]): Pattern.Wildcard[Annotations] =
    Pattern.Wildcard(annotations)

}
