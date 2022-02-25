package zio.morphir.ir

import zio.{Chunk, ZEnvironment}

sealed trait Pattern[+Annotations] {
  def annotations: ZEnvironment[Annotations]
  def mapAttributes[B](f: Annotations => B): Pattern[B] = ???
}

object Pattern {

  def asPattern[Annotations](
      annotations: ZEnvironment[Annotations],
      pattern: Pattern[Annotations],
      name: Name
  ): AsPattern[Annotations] =
    AsPattern(pattern, name, annotations)

  def asPattern(pattern: Pattern[Any], name: Name): AsPattern[Any] =
    AsPattern(pattern, name, ZEnvironment.empty)

  def asPattern(name: Name): AsPattern[Any] =
    AsPattern(wildcardPattern, name, ZEnvironment.empty)

  lazy val wildcardPattern: WildcardPattern[Any] = WildcardPattern[Any](ZEnvironment.empty)

  def wildcardPattern[Annotations](annotations: ZEnvironment[Annotations]): WildcardPattern[Annotations] =
    WildcardPattern(annotations)

  // val unit: UnitPattern[Any] = UnitPattern(ZEnvironment.empty)
  // def unit[Annotations](annotations: ZEnvironment[Annotations]): UnitPattern[Annotations] = UnitPattern(annotations)
  // val wildcard: Wildcard[Any] = Wildcard(ZEnvironment.empty)
  // def wildcard[Annotations](annotations: ZEnvironment[Annotations]): Wildcard[Annotations] = Wildcard(annotations)

  // final case class LiteralPattern[+Annotations, +Value](value: Lit[Value], annotations: ZEnvironment[Annotations])
  //     extends Pattern[Annotations]

  final case class AsPattern[+Annotations](
      pattern: Pattern[Annotations],
      name: Name,
      annotations: ZEnvironment[Annotations]
  ) extends Pattern[Annotations]

  final case class ConstructorPattern[+Annotations](
      constructorName: FQName,
      argumentPatterns: Chunk[Pattern[Annotations]],
      annotations: ZEnvironment[Annotations]
  ) extends Pattern[Annotations]

  final case class EmptyListPattern[+Annotations](annotations: ZEnvironment[Annotations]) extends Pattern[Annotations]

  final case class HeadTailPattern[+Annotations](
      headPattern: Pattern[Annotations],
      tailPattern: Pattern[Annotations],
      annotations: ZEnvironment[Annotations]
  ) extends Pattern[Annotations]

  final case class LiteralPattern[+A, +Annotations](
      literal: Literal[A],
      annotations: ZEnvironment[Annotations]
  ) extends Pattern[Annotations]

  final case class TuplePattern[+Annotations](
      elementPatterns: Chunk[Pattern[Annotations]],
      annotations: ZEnvironment[Annotations]
  ) extends Pattern[Annotations]

  final case class UnitPattern[+Annotations](annotations: ZEnvironment[Annotations]) extends Pattern[Annotations]

  final case class WildcardPattern[+Annotations](annotations: ZEnvironment[Annotations]) extends Pattern[Annotations]

}
