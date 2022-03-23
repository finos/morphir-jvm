package zio.morphir.ir

import zio.Chunk

sealed trait Pattern[+Attributes] { self =>
  import Pattern._

  def attributes: Attributes
  final def mapAttributes[B](f: Attributes => B): Pattern[B] = self match {
    case AsPattern(pattern, name, attributes) => AsPattern(pattern.mapAttributes(f), name, f(attributes))
    case ConstructorPattern(constructorName, argumentPatterns, attributes) =>
      ConstructorPattern(constructorName, argumentPatterns.map(_.mapAttributes(f)), f(attributes))
    case EmptyListPattern(attributes) => EmptyListPattern(f(attributes))
    case HeadTailPattern(headPattern, tailPattern, attributes) =>
      HeadTailPattern(headPattern.mapAttributes(f), tailPattern.mapAttributes(f), f(attributes))
    case LiteralPattern(literal, attributes) => LiteralPattern(literal, f(attributes))
    case TuplePattern(elementPatterns, attributes) =>
      TuplePattern(elementPatterns.map(_.mapAttributes(f)), f(attributes))
    case UnitPattern(attributes)     => UnitPattern(f(attributes))
    case WildcardPattern(attributes) => WildcardPattern(f(attributes))
  }
}

object Pattern {

  def asPattern[Attributes](
      attributes: Attributes,
      pattern: Pattern[Attributes],
      name: Name
  ): AsPattern[Attributes] =
    AsPattern(pattern, name, attributes)

  def asPattern(pattern: Pattern[Any], name: Name): AsPattern[Any] =
    AsPattern(pattern, name, ())

  def asPattern(name: Name): AsPattern[Any] =
    AsPattern(wildcardPattern, name, ())

  lazy val wildcardPattern: WildcardPattern[Any] = WildcardPattern[Any](())

  def wildcardPattern[Attributes](attributes: Attributes): WildcardPattern[Attributes] =
    WildcardPattern(attributes)

  // val unit: UnitPattern[Any] = UnitPattern(ZEnvironment.empty)
  // def unit[Attributes](attributes: ZEnvironment[Attributes]): UnitPattern[Attributes] = UnitPattern(attributes)
  // val wildcard: Wildcard[Any] = Wildcard(ZEnvironment.empty)
  // def wildcard[Attributes](attributes: ZEnvironment[Attributes]): Wildcard[Attributes] = Wildcard(attributes)

  // final case class LiteralPattern[+Attributes, +Value](value: Lit[Value], attributes: ZEnvironment[Attributes])
  //     extends Pattern[Attributes]

  final case class AsPattern[+Attributes](
      pattern: Pattern[Attributes],
      name: Name,
      attributes: Attributes
  ) extends Pattern[Attributes]

  final case class ConstructorPattern[+Attributes](
      constructorName: FQName,
      argumentPatterns: Chunk[Pattern[Attributes]],
      attributes: Attributes
  ) extends Pattern[Attributes]

  final case class EmptyListPattern[+Attributes](attributes: Attributes) extends Pattern[Attributes]

  final case class HeadTailPattern[+Attributes](
      headPattern: Pattern[Attributes],
      tailPattern: Pattern[Attributes],
      attributes: Attributes
  ) extends Pattern[Attributes]

  final case class LiteralPattern[+A, +Attributes](
      literal: Literal[A],
      attributes: Attributes
  ) extends Pattern[Attributes]

  final case class TuplePattern[+Attributes](
      elementPatterns: Chunk[Pattern[Attributes]],
      attributes: Attributes
  ) extends Pattern[Attributes]

  final case class UnitPattern[+Attributes](attributes: Attributes) extends Pattern[Attributes]

  final case class WildcardPattern[+Attributes](attributes: Attributes) extends Pattern[Attributes]

}
