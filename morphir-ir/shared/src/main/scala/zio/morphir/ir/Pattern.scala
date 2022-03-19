package zio.morphir.ir

import zio.Chunk

sealed trait Pattern[+Attributes] { self =>
  import Pattern._

  def annotations: Attributes
  final def mapAttributes[B](f: Attributes => B): Pattern[B] = self match {
    case AsPattern(pattern, name, annotations) => AsPattern(pattern.mapAttributes(f), name, f(annotations))
    case ConstructorPattern(constructorName, argumentPatterns, annotations) =>
      ConstructorPattern(constructorName, argumentPatterns.map(_.mapAttributes(f)), f(annotations))
    case EmptyListPattern(annotations) => EmptyListPattern(f(annotations))
    case HeadTailPattern(headPattern, tailPattern, annotations) =>
      HeadTailPattern(headPattern.mapAttributes(f), tailPattern.mapAttributes(f), f(annotations))
    case LiteralPattern(literal, annotations) => LiteralPattern(literal, f(annotations))
    case TuplePattern(elementPatterns, annotations) =>
      TuplePattern(elementPatterns.map(_.mapAttributes(f)), f(annotations))
    case UnitPattern(annotations)     => UnitPattern(f(annotations))
    case WildcardPattern(annotations) => WildcardPattern(f(annotations))
  }
}

object Pattern {

  def asPattern[Attributes](
      annotations: Attributes,
      pattern: Pattern[Attributes],
      name: Name
  ): AsPattern[Attributes] =
    AsPattern(pattern, name, annotations)

  def asPattern(pattern: Pattern[Any], name: Name): AsPattern[Any] =
    AsPattern(pattern, name, ())

  def asPattern(name: Name): AsPattern[Any] =
    AsPattern(wildcardPattern, name, ())

  lazy val wildcardPattern: WildcardPattern[Any] = WildcardPattern[Any](())

  def wildcardPattern[Attributes](annotations: Attributes): WildcardPattern[Attributes] =
    WildcardPattern(annotations)

  // val unit: UnitPattern[Any] = UnitPattern(ZEnvironment.empty)
  // def unit[Attributes](annotations: ZEnvironment[Attributes]): UnitPattern[Attributes] = UnitPattern(annotations)
  // val wildcard: Wildcard[Any] = Wildcard(ZEnvironment.empty)
  // def wildcard[Attributes](annotations: ZEnvironment[Attributes]): Wildcard[Attributes] = Wildcard(annotations)

  // final case class LiteralPattern[+Attributes, +Value](value: Lit[Value], annotations: ZEnvironment[Attributes])
  //     extends Pattern[Attributes]

  final case class AsPattern[+Attributes](
      pattern: Pattern[Attributes],
      name: Name,
      annotations: Attributes
  ) extends Pattern[Attributes]

  final case class ConstructorPattern[+Attributes](
      constructorName: FQName,
      argumentPatterns: Chunk[Pattern[Attributes]],
      annotations: Attributes
  ) extends Pattern[Attributes]

  final case class EmptyListPattern[+Attributes](annotations: Attributes) extends Pattern[Attributes]

  final case class HeadTailPattern[+Attributes](
      headPattern: Pattern[Attributes],
      tailPattern: Pattern[Attributes],
      annotations: Attributes
  ) extends Pattern[Attributes]

  final case class LiteralPattern[+A, +Attributes](
      literal: Literal[A],
      annotations: Attributes
  ) extends Pattern[Attributes]

  final case class TuplePattern[+Attributes](
      elementPatterns: Chunk[Pattern[Attributes]],
      annotations: Attributes
  ) extends Pattern[Attributes]

  final case class UnitPattern[+Attributes](annotations: Attributes) extends Pattern[Attributes]

  final case class WildcardPattern[+Attributes](annotations: Attributes) extends Pattern[Attributes]

}
