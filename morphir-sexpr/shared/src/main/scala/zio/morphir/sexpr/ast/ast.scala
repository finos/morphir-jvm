package zio.morphir.sexpr.ast

import zio.Chunk
import zio.morphir.sexpr.internal._
import zio.morphir.sexpr.{SExprDecoder, SExprEncoder, SExprError, SExprParser}

sealed trait SExpr {
  self =>

  import SExprCase._

  def $case: SExprCase[SExpr]

  def fold[Z](f: SExprCase[Z] => Z): Z = self.$case match {
    case c @ BoolCase(_)      => f(c)
    case c @ StrCase(_)       => f(c)
    case c @ NumCase(_)       => f(c)
    case c @ SymbolCase(_, _) => f(c)
    case MapCase(items)       => f(MapCase(items.map { case (k, v) => (k.fold(f), v.fold(f)) }))
    case NilCase              => f(NilCase)
    case ConsCase(head, tail) => f(ConsCase(head.fold(f), tail.fold(f)))
    case QuotedCase(get)      => f(QuotedCase(get.fold(f)))
    case VectorCase(items)    => f(VectorCase(items.map(_.fold(f))))
  }

  final def widen: SExpr = this
}

object SExpr {

  import SExprCase._

  def bool(value: Boolean): Bool = Bool(value)

  def symbol(name: String): Symbol   = Symbol(name)
  def vector(items: SExpr*): SVector = SVector(Chunk(items: _*))

  implicit val decoder: SExprDecoder[SExpr] = new SExprDecoder[SExpr] {
    def decode(in: String): Either[SExprError, SExpr] =
      SExprParser.grammar.sexpr.parseString(in).left.map { err =>
        SExprError.ParseError(err.toString)
      }

    override final def fromAST(sexpr: SExpr): Either[String, SExpr] = Right(sexpr)
  }

  implicit val encoder: SExprEncoder[SExpr] = new SExprEncoder[SExpr] {
    def unsafeEncode(a: SExpr, indent: Option[Int], out: Write): Unit =
      a match {
        case j: SVector  => SVector.encoder.unsafeEncode(j, indent, out)
        case j @ SMap(_) => SMap.encoder.unsafeEncode(j.asInstanceOf[SMap[SExpr, SExpr]], indent, out)
        case j: Symbol   => Symbol.encoder.unsafeEncode(j, indent, out)
        case j: Bool     => Bool.encoder.unsafeEncode(j, indent, out)
        case j: Str      => Str.encoder.unsafeEncode(j, indent, out)
        case j: Num      => Num.encoder.unsafeEncode(j, indent, out)
        case Nil         => Nil.encoder.unsafeEncode(Nil, indent, out)
      }

    override final def toAST(a: SExpr): Either[String, SExpr] = Right(a)
  }

  final case class Bool(value: Boolean) extends SExpr {
    lazy val $case: BoolCase = BoolCase(value)
  }

  object Bool {
    object Case {
      def unapply(arg: SExpr): Option[BoolCase] = arg match {
        case sexpr @ Bool(_) => Some(sexpr.$case)
        case _               => None
      }
    }

    val False: Bool = Bool(false)
    val True: Bool  = Bool(true)

    implicit val decoder: SExprDecoder[Bool] = new SExprDecoder[Bool] {
      def decode(in: String): Either[SExprError, Bool] =
        SExprParser.grammar.bool.parseString(in).left.map { err =>
          SExprError.ParseError(err.toString)
        }

      override final def fromAST(sexpr: SExpr): Either[String, Bool] =
        sexpr match {
          case b: Bool => Right(b)
          case _       => Left(s"Not a bool value")
        }
    }

    implicit val encoder: SExprEncoder[Bool] = new SExprEncoder[Bool] {
      def unsafeEncode(a: Bool, indent: Option[Int], out: Write): Unit =
        SExprEncoder.boolean.unsafeEncode(a.$case.value, indent, out)

      override final def toAST(a: Bool): Either[String, SExpr] = Right(a)
    }
  }

  case object Nil extends SExpr {
    lazy val $case = NilCase

    implicit val decoder: SExprDecoder[Nil.type] = new SExprDecoder[Nil.type] {
      def decode(in: String): Either[SExprError, Nil.type] =
        SExprParser.grammar.nil.parseString(in).left.map { err =>
          SExprError.ParseError(err.toString)
        }

      override final def fromAST(sexpr: SExpr): Either[String, Nil.type] =
        sexpr match {
          case Nil => Right(Nil)
          case _   => Left(s"Not nil")
        }
    }

    implicit val encoder: SExprEncoder[Nil.type] = new SExprEncoder[Nil.type] {
      def unsafeEncode(a: Nil.type, indent: Option[Int], out: Write): Unit =
        out.write("nil")

      override final def toAST(a: Nil.type): Either[String, SExpr] = Right(a)
    }
  }

  final case class Num(value: java.math.BigDecimal) extends SExpr {
    lazy val $case: NumCase = NumCase(value)
  }

  object Num {
    def apply(value: BigInt): Num               = Num(new java.math.BigDecimal(value.bigInteger))
    def apply(value: java.math.BigInteger): Num = Num(new java.math.BigDecimal(value))
    def apply(value: BigDecimal): Num           = Num(value.bigDecimal)
    def apply(value: Byte): Num                 = Num(BigDecimal(value.toInt).bigDecimal)
    def apply(value: Double): Num               = Num(BigDecimal(value).bigDecimal)
    def apply(value: Float): Num                = Num(BigDecimal(value.toDouble).bigDecimal)
    def apply(value: Int): Num                  = Num(BigDecimal(value).bigDecimal)
    def apply(value: Long): Num                 = Num(BigDecimal(value).bigDecimal)
    def apply(value: Short): Num                = Num(BigDecimal(value.toInt).bigDecimal)

    object Case {
      def unapply(exp: SExpr): Option[java.math.BigDecimal] = exp.$case match {
        case NumCase(value) => Some(value)
        case _              => None
      }
    }

    implicit val decoder: SExprDecoder[Num] = new SExprDecoder[Num] {
      def decode(in: String): Either[SExprError, Num] =
        SExprParser.grammar.num.parseString(in).left.map { err =>
          SExprError.ParseError(err.toString)
        }

      override final def fromAST(sexpr: SExpr): Either[String, Num] =
        sexpr match {
          case b: Num => Right(b)
          case _      => Left(s"Not a number")
        }
    }

    implicit val encoder: SExprEncoder[Num] = new SExprEncoder[Num] {
      def unsafeEncode(a: Num, indent: Option[Int], out: Write): Unit =
        SExprEncoder.bigDecimal.unsafeEncode(a.$case.value, indent, out)

      override final def toAST(a: Num): Either[String, Num] = Right(a)
    }
  }

  final case class Str(value: String) extends SExpr {
    lazy val $case: StrCase = StrCase(value)
  }

  final case class SMap[K <: SExpr, V <: SExpr](items: Map[K, V]) extends SExpr {
    def $case: MapCase[SExpr] = MapCase(items.asInstanceOf[Map[SExpr, SExpr]])
  }

  object SMap {
    object Case {
      def unapply(arg: SExpr): Option[Map[SExpr, SExpr]] = arg.$case match {
        case MapCase(items: Map[SExpr, SExpr]) => Some(items)
        case _                                 => None
      }
    }

    implicit val decoder: SExprDecoder[SMap[SExpr, SExpr]] = new SExprDecoder[SMap[SExpr, SExpr]] {
      def decode(in: String): Either[SExprError, SMap[SExpr, SExpr]] =
        SExprParser.grammar.map.parseString(in).left.map { err =>
          SExprError.ParseError(err.toString)
        }

      override final def fromAST(sexpr: SExpr): Either[String, SMap[SExpr, SExpr]] =
        sexpr match {
          case m @ SMap(_) => Right(m.asInstanceOf[SMap[SExpr, SExpr]])
          case _           => Left(s"Not a map")
        }
    }

    private lazy val mapE = SExprEncoder.map[SExpr, SExpr]
    implicit val encoder: SExprEncoder[SMap[SExpr, SExpr]] = new SExprEncoder[SMap[SExpr, SExpr]] {
      def unsafeEncode(map: SMap[SExpr, SExpr], indent: Option[Int], out: Write): Unit =
        mapE.unsafeEncode(map.$case.items, indent, out)

      override final def toAST(a: SMap[SExpr, SExpr]): Either[String, SExpr] = Right(a)
    }
  }

  object Str {
    object Case {
      def unapply(exp: SExpr): Option[String] = exp.$case match {
        case StrCase(value) => Some(value)
        case _              => None
      }
    }

    implicit val decoder: SExprDecoder[Str] = new SExprDecoder[Str] {
      def decode(in: String): Either[SExprError, Str] =
        SExprParser.grammar.str.parseString(in).left.map { err =>
          SExprError.ParseError(err.toString)
        }

      override final def fromAST(sexpr: SExpr): Either[String, Str] =
        sexpr match {
          case b: Str => Right(b)
          case _      => Left(s"Not a String value")
        }
    }

    implicit val encoder: SExprEncoder[Str] = new SExprEncoder[Str] {
      def unsafeEncode(a: Str, indent: Option[Int], out: Write): Unit =
        SExprEncoder.string.unsafeEncode(a.$case.value, indent, out)

      override final def toAST(a: Str): Either[String, SExpr] = Right(a)
    }
  }

  final case class SVector(items: Chunk[SExpr]) extends SExpr {
    lazy val $case: VectorCase[SExpr] = VectorCase(items)
  }

  object SVector {
    object Case {
      def unapply(arg: SExpr): Option[Chunk[SExpr]] = arg.$case match {
        case VectorCase(items) => Some(items)
        case _                 => None
      }
    }

    implicit val decoder: SExprDecoder[SVector] = new SExprDecoder[SVector] {
      def decode(in: String): Either[SExprError, SVector] =
        SExprParser.grammar.vector.parseString(in).left.map { err =>
          SExprError.ParseError(err.toString)
        }

      override final def fromAST(SExpr: SExpr): Either[String, SVector] =
        SExpr match {
          case vect: SVector => Right(vect)
          case _             => Left(s"Not a vector")
        }
    }

    private lazy val vactE = SExprEncoder.chunk[SExpr]
    implicit val encoder: SExprEncoder[SVector] = new SExprEncoder[SVector] {
      def unsafeEncode(a: SVector, indent: Option[Int], out: Write): Unit =
        vactE.unsafeEncode(a.$case.items, indent, out)

      override final def toAST(a: SVector): Either[String, SExpr] = Right(a)
    }
  }

  final case class Symbol(value: String, kind: SymbolKind) extends SExpr {
    def $case: SymbolCase = SymbolCase(value, kind)
  }

  object Symbol {
    def apply(name: String): Symbol = name match {
      case s if s.startsWith("::") => Symbol(name, SymbolKind.Macro)
      case s if s.startsWith(":")  => Symbol(name, SymbolKind.Keyword)
      case name                    => Symbol(name, SymbolKind.Standard)
    }

    object Case {
      def unapply(arg: SExpr): Option[SymbolCase] = arg.$case match {
        case c @ SymbolCase(_, _) => Some(c)
        case _                    => None
      }
    }

    implicit val decoder: SExprDecoder[Symbol] = new SExprDecoder[Symbol] {
      def decode(in: String): Either[SExprError, Symbol] =
        SExprParser.grammar.symbol.parseString(in).left.map { err =>
          SExprError.ParseError(err.toString)
        }

      override final def fromAST(sexpr: SExpr): Either[String, Symbol] =
        sexpr match {
          case b: Symbol => Right(b)
          case _         => Left(s"Not a Symbol value")
        }
    }

    implicit val encoder: SExprEncoder[Symbol] = new SExprEncoder[Symbol] {
      def unsafeEncode(a: Symbol, indent: Option[Int], out: Write): Unit =
        SExprEncoder.symbol.contramap[Symbol](ast => scala.Symbol(ast.value)).unsafeEncode(a, indent, out)

      override final def toAST(a: Symbol): Either[String, Symbol] = Right(a)
    }
  }
}

sealed trait SExprCase[+Self] {
  self =>

  import SExprCase._

  def map[B](f: Self => B): SExprCase[B] = self match {
    case BoolCase(value)         => BoolCase(value)
    case ConsCase(head, tail)    => ConsCase(f(head), f(tail))
    case StrCase(value)          => StrCase(value)
    case SymbolCase(value, kind) => SymbolCase(value, kind)
    case MapCase(items)          => MapCase(items.map { case (k, v) => (f(k), f(v)) })
    case NilCase                 => NilCase
    case NumCase(value)          => NumCase(value)
    case QuotedCase(get)         => QuotedCase(f(get))
    case VectorCase(items)       => VectorCase(items.map(f))
  }
}

object SExprCase {
  sealed trait AtomCase[+Self] extends SExprCase[Self]

  sealed trait CollectionCase[+Self] extends SExprCase[Self]

  sealed trait ListCase[+Self] extends CollectionCase[Self]

  sealed trait SymbolBaseCase[+Self] extends AtomCase[Self]

  // Leaf Cases
  final case class BoolCase(value: Boolean) extends SymbolBaseCase[Nothing]

  case object NilCase extends ListCase[Nothing]

  final case class NumCase(value: java.math.BigDecimal) extends AtomCase[Nothing]

  final case class SymbolCase(name: String, kind: SymbolKind) extends SymbolBaseCase[Nothing]
  object SymbolCase {
    def apply(value: String): SymbolCase = SymbolCase(value, SymbolKind.Standard)
  }

  final case class StrCase(value: String) extends AtomCase[Nothing]

  // Recursive Cases
  final case class ConsCase[+Self](head: Self, tail: Self) extends ListCase[Self]

  final case class MapCase[Self](items: Map[Self, Self]) extends CollectionCase[Self]

  final case class QuotedCase[+Self](get: Self) extends SExprCase[Self]

  final case class VectorCase[+Self](items: Chunk[Self]) extends CollectionCase[Self]
}

sealed abstract class SymbolKind(val isMacro: Boolean, val isKeyword: Boolean)
object SymbolKind {
  case object Standard extends SymbolKind(isMacro = false, isKeyword = false)
  case object Keyword  extends SymbolKind(isMacro = false, isKeyword = true)
  case object Macro    extends SymbolKind(isMacro = true, isKeyword = true)
}
