package zio.morphir.sexpr.ast

import zio.Chunk
import zio.morphir.sexpr.{SExprDecoder, SExprEncoder}

sealed trait SExpr { self =>
  import SExprCase._
  def $case: SExprCase[SExpr]

  def fold[Z](f: SExprCase[Z] => Z): Z = self.$case match {
    case c @ BoolCase(_)      => f(c)
    case c @ StrCase(_)       => f(c)
    case c @ NumCase(_)       => f(c)
    case c @ SymbolCase(_)    => f(c)
    case MapCase(items)       =>
      f(MapCase(items.map { case (k, v) =>
        (k.fold(f), v.fold(f))
      }))
    case NilCase              => f(NilCase)
    case ConsCase(head, tail) => f(ConsCase(head.fold(f), tail.fold(f)))
    case QuotedCase(get)      => f(QuotedCase(get.fold(f)))
    case VectorCase(items)    => f(VectorCase(items.map(_.fold(f))))
  }

  final def widen: SExpr = this
}

object SExpr {
  import SExprCase._

  implicit val decoder: SExprDecoder[SExpr] = ???

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

  final case class SMap private[sexpr] ($case: VectorCase[SExpr]) extends SExpr
  object SMap {
    def apply(items: Map[SExpr, SExpr]): SMap          = SMap(items)
    def unapply(arg: SExpr): Option[Map[SExpr, SExpr]] = arg.$case match {
      case MapCase(items: Map[SExpr, SExpr]) => Some(items)
      case _                                 => None
    }
  }

  case object Nil extends SExpr {
    val $case = NilCase
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

sealed trait SExprCase[+Self] { self =>
  import SExprCase._

  def map[B](f: Self => B): SExprCase[B] = self match {
    case BoolCase(value)      => BoolCase(value)
    case ConsCase(head, tail) => ConsCase(f(head), f(tail))
    case StrCase(value)       => StrCase(value)
    case SymbolCase(value)    => SymbolCase(value)
    case MapCase(items)       =>
      MapCase(items.map { case (k, v) =>
        (f(k), f(v))
      })
    case NilCase              => NilCase
    case NumCase(value)       => NumCase(value)
    case QuotedCase(get)      => QuotedCase(f(get))
    case VectorCase(items)    => VectorCase(items.map(f))
  }

}
object SExprCase {
  sealed trait AtomCase[+Self]       extends SExprCase[Self]
  sealed trait CollectionCase[+Self] extends SExprCase[Self]
  sealed trait ListCase[+Self]       extends CollectionCase[Self]
  sealed trait SymbolBaseCase[+Self] extends AtomCase[Self]

  // Leaf Cases
  final case class BoolCase(value: Boolean)             extends SymbolBaseCase[Nothing]
  final case class StrCase(value: String)               extends AtomCase[Nothing]
  final case class NumCase(value: java.math.BigDecimal) extends AtomCase[Nothing]
  case object NilCase                                   extends ListCase[Nothing]
  final case class SymbolCase(value: String)            extends SymbolBaseCase[Nothing]

  // Recursive Cases
  final case class ConsCase[+Self](head: Self, tail: Self) extends ListCase[Self]
  final case class MapCase[Self](items: Map[Self, Self])   extends CollectionCase[Self]
  final case class QuotedCase[+Self](get: Self)            extends SExprCase[Self]
  final case class VectorCase[+Self](items: Chunk[Self])   extends CollectionCase[Self]

}
