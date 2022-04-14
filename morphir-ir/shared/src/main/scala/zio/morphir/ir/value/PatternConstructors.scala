package zio.morphir.ir.value

import zio.Chunk
import zio.morphir.Not
import zio.morphir.ir.value.Pattern.DefaultAttributes
import zio.morphir.ir.{FQName, Literal, Name}

trait PatternConstructors { self =>
  final def asAlias[A](attributes: A, alias: String): Pattern[A] =
    Pattern.AsPattern(
      attributes = attributes,
      pattern = Pattern.WildcardPattern(attributes),
      name = Name.fromString(alias)
    )

  final def asAlias[A](attributes: A, alias: Name): Pattern[A] =
    Pattern.AsPattern(
      attributes = attributes,
      pattern = Pattern.WildcardPattern(attributes),
      name = alias
    )

  final def asAlias(alias: String): UPattern =
    Pattern.AsPattern(
      attributes = DefaultAttributes,
      pattern = wildcardPattern,
      name = Name.fromString(alias)
    )

  final def asAlias(alias: Name): UPattern =
    Pattern.AsPattern(
      attributes = DefaultAttributes,
      pattern = wildcardPattern,
      name = alias
    )

  final def asPattern[A](attributes: A, pattern: Pattern[A], alias: Name): Pattern[A] =
    Pattern.AsPattern(attributes = attributes, pattern = pattern, name = alias)

  final def asPattern[A](attributes: A, pattern: Pattern[A], alias: String): Pattern[A] =
    Pattern.AsPattern(attributes = attributes, pattern = pattern, name = Name.fromString(alias))

  final def asPattern(pattern: UPattern, alias: Name): UPattern =
    Pattern.AsPattern(attributes = DefaultAttributes, pattern = pattern, name = alias)

  final def asPattern(pattern: UPattern, alias: String): UPattern =
    Pattern.AsPattern(attributes = DefaultAttributes, pattern = pattern, name = Name.fromString(alias))

  final def asPattern(alias: String): UPattern =
    Pattern.AsPattern(
      attributes = DefaultAttributes,
      pattern = wildcardPattern,
      name = Name.fromString(alias)
    )

  final def asPattern(alias: Name): UPattern =
    Pattern.AsPattern(
      attributes = DefaultAttributes,
      pattern = wildcardPattern,
      name = alias
    )

  final def booleanPattern[A](attributes: A, value: Boolean): Pattern[A] =
    Pattern.LiteralPattern(attributes = attributes, literal = Literal.boolean(value))

  final def booleanPattern(value: Boolean): UPattern =
    Pattern.LiteralPattern(attributes = DefaultAttributes, literal = Literal.boolean(value))

  final def constructorPattern[A](
      attributes: A,
      constructorName: FQName,
      argumentPatterns: Chunk[Pattern[A]]
  ): Pattern[A] =
    Pattern.ConstructorPattern(
      attributes = attributes,
      constructorName = constructorName,
      argumentPatterns = argumentPatterns
    )

  final def constructorPattern[A](
      attributes: A,
      constructorName: String,
      argumentPatterns: Chunk[Pattern[A]]
  ): Pattern[A] =
    Pattern.ConstructorPattern(
      attributes = attributes,
      constructorName = FQName.fromString(constructorName),
      argumentPatterns = argumentPatterns
    )

  final def decimalPattern[A](attributes: A, value: BigDecimal): Pattern[A] =
    Pattern.LiteralPattern(attributes = attributes, literal = Literal.decimal(value))

  final def decimalPattern(value: BigDecimal): UPattern =
    Pattern.LiteralPattern(attributes = DefaultAttributes, literal = Literal.decimal(value))

  final def emptyListPattern[A](attributes: A): Pattern[A] =
    Pattern.EmptyListPattern(attributes)

  final lazy val emptyListPattern: UPattern =
    Pattern.EmptyListPattern(DefaultAttributes)

  final def falsePattern[A](attributes: A): Pattern[A] =
    Pattern.LiteralPattern(attributes = attributes, literal = Literal.False)

  final def falsePattern: UPattern =
    Pattern.LiteralPattern(attributes = DefaultAttributes, literal = Literal.False)

  final def floatPattern[A](attributes: A, value: Float): Pattern[A] =
    Pattern.LiteralPattern(attributes = attributes, literal = Literal.float(value))

  final def floatPattern(value: Float): UPattern =
    Pattern.LiteralPattern(attributes = DefaultAttributes, literal = Literal.float(value))

  final def headTailPattern[A](attributes: A, head: Pattern[A], tail: Pattern[A]): Pattern[A] =
    Pattern.HeadTailPattern(attributes = attributes, headPattern = head, tailPattern = tail)

  final def headTailPattern(head: UPattern, tail: UPattern): UPattern =
    Pattern.HeadTailPattern(attributes = DefaultAttributes, headPattern = head, tailPattern = tail)

  final def intPattern[A](attributes: A, value: Int): Pattern[A] =
    Pattern.LiteralPattern(attributes = attributes, literal = Literal.int(value))

  final def intPattern(value: Int): UPattern =
    Pattern.LiteralPattern(attributes = DefaultAttributes, literal = Literal.int(value))

  final def literalPattern[A, T](attributes: A, value: Literal[T]): Pattern[A] =
    Pattern.LiteralPattern(attributes = attributes, literal = value)

  final def literalPattern[T](value: Literal[T]): UPattern =
    Pattern.LiteralPattern(attributes = DefaultAttributes, literal = value)

  final def stringPattern[A](attributes: A, value: String): Pattern[A] =
    Pattern.LiteralPattern(attributes = attributes, literal = Literal.string(value))

  final def stringPattern(value: String): UPattern =
    Pattern.LiteralPattern(attributes = DefaultAttributes, literal = Literal.string(value))

  final def truePattern[A](attributes: A): Pattern[A] =
    Pattern.LiteralPattern(attributes = attributes, literal = Literal.True)

  final def truePattern: UPattern =
    Pattern.LiteralPattern(attributes = DefaultAttributes, literal = Literal.True)

  final def tuplePattern[A](attributes: A, patterns: Chunk[Pattern[A]]): Pattern[A] =
    Pattern.TuplePattern(attributes = attributes, elementPatterns = patterns)

  final def tuplePattern[A](attributes: A, patterns: Pattern[A]*)(implicit ev: Not[A <:< Pattern[_]]): Pattern[A] =
    Pattern.TuplePattern(attributes = attributes, elementPatterns = Chunk.fromIterable(patterns))

  final def tuplePattern(patterns: Chunk[UPattern]): UPattern =
    Pattern.TuplePattern(attributes = DefaultAttributes, elementPatterns = patterns)

  final def tuplePattern(patterns: UPattern*): UPattern =
    Pattern.TuplePattern(attributes = DefaultAttributes, elementPatterns = Chunk.fromIterable(patterns))

  final def wildcardPattern[A](attributes: A): Pattern[A] = Pattern.WildcardPattern(attributes)

  final lazy val wildcardPattern: UPattern = Pattern.WildcardPattern(Pattern.DefaultAttributes)

}
