package zio.morphir.ir.value

import zio.Chunk
import zio.morphir.ir.Type.UType
import zio.morphir.ir.{FQName, Literal, Name}
sealed trait Pattern[+A] { self =>
  import Pattern._

  def attributes: A
  final def mapAttributes[B](f: A => B): Pattern[B] = self match {
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

  def withAttributes[B >: A](attributes: => B): Pattern[B] =
    self.mapAttributes(_ => attributes)
}

object Pattern {
  type DefaultAttributes = Any
  val DefaultAttributes: DefaultAttributes = ()

  def asPattern[Attributes](
      attributes: Attributes,
      pattern: Pattern[Attributes],
      name: Name
  ): AsPattern[Attributes] =
    AsPattern(pattern, name, attributes)

  def asPattern(pattern: UPattern, name: Name): UPattern =
    AsPattern(pattern, name, DefaultAttributes)

  def asPattern(name: Name): UPattern =
    AsPattern(wildcardPattern, name, DefaultAttributes)

  lazy val wildcardPattern: WildcardPattern[Any] = WildcardPattern(DefaultAttributes)

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

  object AsPattern {
    type Raw = AsPattern[Any]
    object Raw {
      def apply(pattern: UPattern, name: Name): Raw   = AsPattern(pattern, name, DefaultAttributes)
      def apply(pattern: UPattern, name: String): Raw = AsPattern(pattern, Name.fromString(name), DefaultAttributes)
    }
  }

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

  object LiteralPattern {
    type Raw[+A] = LiteralPattern[A, DefaultAttributes]
    object Raw {
      def apply[A](value: Literal[A]): Raw[A] = LiteralPattern(value, DefaultAttributes)
    }
    type Typed[+A] = LiteralPattern[A, UType]
    object Typed {
      def apply[A](literal: Literal[A])(ascribedType: UType): LiteralPattern[A, UType] =
        LiteralPattern(literal, ascribedType)
    }
  }

  final case class TuplePattern[+Attributes](
      elementPatterns: Chunk[Pattern[Attributes]],
      attributes: Attributes
  ) extends Pattern[Attributes]

  final case class UnitPattern[+Attributes](attributes: Attributes) extends Pattern[Attributes]

  final case class WildcardPattern[+Attributes](attributes: Attributes) extends Pattern[Attributes]
  object WildcardPattern {
    val raw: Raw = WildcardPattern.Raw()
    type Raw = WildcardPattern[DefaultAttributes]
    object Raw {
      def apply(): Raw = WildcardPattern(DefaultAttributes)
    }
  }

  final implicit class UPatternExtensions(private val self: Pattern[Any]) extends AnyVal {
    def :@(ascribedType: UType): Pattern[UType] = self.mapAttributes((_ => ascribedType))
  }
}
