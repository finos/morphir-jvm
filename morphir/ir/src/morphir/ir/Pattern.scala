package morphir.ir
import morphir.ir.syntax.all._
sealed abstract class Pattern[+A] extends Product with Serializable {
  def attributes: A
  def mapAttributes[B](f: A => B): Pattern[B]
}

object Pattern {
  final case class WildcardPattern[+A](attributes: A) extends Pattern[A] {
    def mapAttributes[B](f: A => B): Pattern[B] = WildcardPattern(f(attributes))
  }
  final case class AsPattern[+A](attributes: A, pattern: Pattern[A], name: Name) extends Pattern[A] {
    def mapAttributes[B](f: A => B): Pattern[B] = AsPattern(f(attributes), pattern mapAttributes f, name)
  }
  final case class TuplePattern[+A](attributes: A, elementPatterns: PatternList[A]) extends Pattern[A] {
    def mapAttributes[B](f: A => B): Pattern[B] = TuplePattern(f(attributes), elementPatterns.mapAttributes(f))
  }
  final case class RecordPattern[+A](attributes: A, fieldNames: List[Name]) extends Pattern[A] {
    def mapAttributes[B](f: A => B): Pattern[B] = RecordPattern(f(attributes), fieldNames)
  }
  final case class ConstructorPattern[+A](attributes: A, constructorName: FQName, argumentPatterns: PatternList[A])
      extends Pattern[A] {
    def mapAttributes[B](f: A => B): Pattern[B] =
      ConstructorPattern(f(attributes), constructorName, argumentPatterns.mapAttributes(f))
  }
  final case class EmptyListPattern[+A](attributes: A) extends Pattern[A] {
    def mapAttributes[B](f: A => B): Pattern[B] = EmptyListPattern(f(attributes))
  }
  final case class HeadTailPattern[+A](attributes: A, headPattern: Pattern[A], tailPattern: Pattern[A])
      extends Pattern[A] {
    def mapAttributes[B](f: A => B): Pattern[B] =
      HeadTailPattern(f(attributes), headPattern.mapAttributes(f), tailPattern.mapAttributes(f))
  }
  final case class LiteralPattern[+A](attributes: A, value: LiteralValue) extends Pattern[A] {
    def mapAttributes[B](f: A => B): Pattern[B] = LiteralPattern(f(attributes), value)
  }
  final case class UnitPattern[+A](attributes: A) extends Pattern[A] {
    def mapAttributes[B](f: A => B): Pattern[B] = UnitPattern(f(attributes))
  }
}
