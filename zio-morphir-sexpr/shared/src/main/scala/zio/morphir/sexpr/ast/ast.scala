package zio.morphir.sexpr.ast

import zio.Chunk
import zio.morphir.sexpr.internal.*
import zio.morphir.sexpr.SExprEncoder

sealed trait SExpr { self =>
  import SExpr.*
  import SExprCase.*
  def $case: SExprCase[SExpr]

  def fold[Z](f: SExprCase[Z] => Z): Z = self.$case match {
    case c @ BoolCase(_)    => f(c)
    case c @ StrCase(_)     => f(c)
    case c @ NumCase(_)     => f(c)
    case NilCase            => f(NilCase)
    case ConsCase(car, cdr) => f(ConsCase(car.fold(f), cdr.fold(f)))
    case QuotedCase(value)  => f(QuotedCase(value.fold(f)))
    case VectorCase(items)  => f(VectorCase(items.map(_.fold(f))))
  }

  final def widen: SExpr = this
}

object SExpr {
  import SExprCase.*

  implicit val encoder: SExprEncoder[SExpr] = SExprEncoder.fromFunction {
    case (sexpr: Bool, indent, out) => ???
    case _                          => ???
  }

  def bool(value: Boolean): Bool = Bool(value)

  def vector(items: SExpr*): SVector = SVector(Chunk(items: _*))

  final case class Bool private[sexpr] ($case: BoolCase) extends SExpr
  object Bool {
    def apply(value: Boolean): Bool = Bool(BoolCase(value))
    def unapply(arg: SExpr): Option[Boolean] = arg.$case match {
      case BoolCase(value) => Some(value)
      case _               => None
    }
  }

  final case class Num private[ast] ($case: NumCase) extends SExpr
  object Num {
    def apply(value: java.math.BigDecimal): Num = Num(NumCase(value))
    def unapply(exp: SExpr): Option[java.math.BigDecimal] = exp.$case match {
      case NumCase(value) => Some(value)
      case _              => None
    }
  }

  final case class Str private[ast] ($case: StrCase) extends SExpr
  object Str {
    def apply(value: String): Str = Str(StrCase(value))
    def unapply(exp: SExpr): Option[String] = exp.$case match {
      case StrCase(value) => Some(value)
      case _              => None
    }
  }

  final case class SVector private[sexpr] ($case: VectorCase[SExpr]) extends SExpr
  object SVector {
    def apply(items: Chunk[SExpr]): SVector = SVector(VectorCase(items))
    def unapply(arg: SExpr): Option[Chunk[SExpr]] = arg.$case match {
      case VectorCase(items) => Some(items)
      case _                 => None
    }
  }
}

sealed trait SExprCase[+A] { self =>
  import SExprCase.*
  def map[B](f: A => B): SExprCase[B] = self match {
    case BoolCase(value)    => BoolCase(value)
    case ConsCase(car, cdr) => ConsCase(f(car), f(cdr))
    case StrCase(value)     => StrCase(value)
    case NilCase            => NilCase
    case NumCase(value)     => NumCase(value)
    case QuotedCase(value)  => QuotedCase(f(value))
    case VectorCase(items)  => VectorCase(items.map(f))
  }

}
object SExprCase {
  sealed trait AtomCase[+A]       extends SExprCase[A]
  sealed trait CollectionCase[+A] extends SExprCase[A]
  sealed trait ListCase[+A]       extends CollectionCase[A]
  sealed trait SymbolCase[+A]     extends AtomCase[A]

  // Leaf Cases
  final case class BoolCase(value: Boolean)             extends SymbolCase[Nothing]
  final case class StrCase(value: String)               extends AtomCase[Nothing]
  final case class NumCase(value: java.math.BigDecimal) extends AtomCase[Nothing]
  case object NilCase                                   extends ListCase[Nothing]

  // Recursive Cases
  final case class ConsCase[+A](car: A, cdr: A)    extends ListCase[A]
  final case class QuotedCase[+A](value: A)        extends SExprCase[A]
  final case class VectorCase[+A](items: Chunk[A]) extends CollectionCase[A]

}
