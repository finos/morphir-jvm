package zio.morphir.sexpr

import scala.annotation._
import zio.morphir.sexpr.ast.SExpr
import zio.morphir.sexpr.internal._

import java.math.{BigDecimal, BigInteger}
import java.util.UUID
import scala.math.{BigDecimal => ScalaBigDecimal, BigInt => ScalaBigInt}

trait SExprEncoder[A] { self =>

  /**
   * Returns a new encoder, with a new input type, which can be transformed to the old input type by the specified
   * user-defined function.
   */
  final def contramap[B](f: B => A): SExprEncoder[B] = new SExprEncoder[B] {

    override def unsafeEncode(b: B, indent: Option[Int], out: Write): Unit =
      self.unsafeEncode(f(b), indent, out)

    override def isNothing(b: B): Boolean = self.isNothing(f(b))

    override final def toAST(b: B): Either[String, SExpr] =
      self.toAST(f(b))
  }

  /**
   * Encodes the specified value into a SExpr string, with the specified indentation level.
   */
  final def encodeSExpr(a: A, indent: Option[Int]): CharSequence = {
    val writer = new FastStringWrite(64)
    unsafeEncode(a, indent, writer)
    writer.buffer
  }

  /**
   * This default may be overriden when this value may be missing within a JSON object and still be encoded.
   */
  @nowarn("msg=is never used")
  def isNothing(a: A): Boolean = false

  @nowarn("msg=is never used")
  def xmap[B](f: A => B, g: B => A): SExprEncoder[B] = contramap(g)

  def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit

  def toAST(a: A): Either[String, SExpr] =
    // SExpr.decoder.decodeJson(encodeJson(a, None))
    ???

  /**
   * Returns this encoder but narrowed to the its given sub-type
   */
  final def narrow[B <: A]: SExprEncoder[B] = self.asInstanceOf[SExprEncoder[B]]

}

object SExprEncoder extends EncoderLowPriority1 {
  def apply[A](implicit encoder: SExprEncoder[A]): SExprEncoder[A] = encoder
  def fromFunction[A](encodeFn: (A, Option[Int], Write) => Unit): SExprEncoder[A] =
    new SExprEncoder[A] {
      def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = encodeFn(a, indent, out)
    }

  implicit val string: SExprEncoder[String] = new SExprEncoder[String] {

    override def unsafeEncode(a: String, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      var i   = 0
      val len = a.length
      while (i < len) {
        (a.charAt(i): @switch) match {
          case '"'  => out.write("\\\"")
          case '\\' => out.write("\\\\")
          case '\b' => out.write("\\b")
          case '\f' => out.write("\\f")
          case '\n' => out.write("\\n")
          case '\r' => out.write("\\r")
          case '\t' => out.write("\\t")
          case c =>
            if (c < ' ') out.write("\\u%04x".format(c.toInt))
            else out.write(c)
        }
        i += 1
      }
      out.write('"')
    }

    override final def toAST(a: String): Either[String, SExpr] =
      Right(SExpr.Str(a))
  }

  implicit val char: SExprEncoder[Char] = new SExprEncoder[Char] {
    override def unsafeEncode(a: Char, indent: Option[Int], out: Write): Unit = {
      out.write('"')

      (a: @switch) match {
        case '"'  => out.write("\\\"")
        case '\\' => out.write("\\\\")
        case c =>
          if (c < ' ') out.write("\\u%04x".format(c.toInt))
          else out.write(c)
      }
      out.write('"')
    }

    override def toAST(a: Char): Either[String, SExpr] =
      Right(SExpr.Str(a.toString))
  }

  private[sexpr] def explicit[A](f: A => String, g: A => SExpr): SExprEncoder[A] =
    new SExprEncoder[A] {
      def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = out.write(f(a))

      override final def toAST(a: A): Either[String, SExpr] =
        Right(g(a))
    }

  private[sexpr] def stringify[A](f: A => String): SExprEncoder[A] = new SExprEncoder[A] {
    def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit = {
      out.write('"')
      out.write(f(a))
      out.write('"')
    }

    override final def toAST(a: A): Either[String, SExpr] =
      Right(SExpr.Str(f(a)))
  }

  implicit val boolean: SExprEncoder[Boolean] = explicit(_.toString, SExpr.Bool.apply)
  implicit val byte: SExprEncoder[Byte] =
    explicit(_.toString, n => SExpr.Num(new java.math.BigDecimal(n.toInt)))
  implicit val int: SExprEncoder[Int] =
    explicit(_.toString, n => SExpr.Num(new java.math.BigDecimal(n)))
  implicit val short: SExprEncoder[Short] =
    explicit(_.toString, n => SExpr.Num(new java.math.BigDecimal(n.toInt)))
  implicit val long: SExprEncoder[Long] =
    explicit(_.toString, n => SExpr.Num(new java.math.BigDecimal(n)))
  implicit val bigInteger: SExprEncoder[BigInteger] =
    explicit(_.toString, n => SExpr.Num(new java.math.BigDecimal(n)))
  implicit val scalaBigInt: SExprEncoder[ScalaBigInt] =
    explicit(_.toString, n => SExpr.Num(new java.math.BigDecimal(n.bigInteger)))
  implicit val double: SExprEncoder[Double] =
    explicit(SafeNumbers.toString, n => SExpr.Num(new java.math.BigDecimal(n)))
  implicit val float: SExprEncoder[Float] =
    explicit(SafeNumbers.toString, n => SExpr.Num(new java.math.BigDecimal(n.toDouble)))
  implicit val bigDecimal: SExprEncoder[BigDecimal] = explicit(_.toString, SExpr.Num.apply)
  implicit val scalaBigDecimal: SExprEncoder[ScalaBigDecimal] =
    explicit(_.toString, n => SExpr.Num(n.bigDecimal))

}

private[sexpr] trait EncoderLowPriority1 extends EncoderLowPriority2 {
  self: SExprEncoder.type =>
}

private[sexpr] trait EncoderLowPriority2 extends EncoderLowPriority3 {
  self: SExprEncoder.type =>
}

private[sexpr] trait EncoderLowPriority3 {
  self: SExprEncoder.type =>

  import java.time._

  implicit val dayOfWeek: SExprEncoder[DayOfWeek]           = stringify(_.toString)
  implicit val duration: SExprEncoder[Duration]             = stringify(serializers.toString)
  implicit val instant: SExprEncoder[Instant]               = stringify(serializers.toString)
  implicit val localDate: SExprEncoder[LocalDate]           = stringify(serializers.toString)
  implicit val localDateTime: SExprEncoder[LocalDateTime]   = stringify(serializers.toString)
  implicit val localTime: SExprEncoder[LocalTime]           = stringify(serializers.toString)
  implicit val month: SExprEncoder[Month]                   = stringify(_.toString)
  implicit val monthDay: SExprEncoder[MonthDay]             = stringify(serializers.toString)
  implicit val offsetDateTime: SExprEncoder[OffsetDateTime] = stringify(serializers.toString)
  implicit val offsetTime: SExprEncoder[OffsetTime]         = stringify(serializers.toString)
  implicit val period: SExprEncoder[Period]                 = stringify(serializers.toString)
  implicit val year: SExprEncoder[Year]                     = stringify(serializers.toString)
  implicit val yearMonth: SExprEncoder[YearMonth]           = stringify(serializers.toString)
  implicit val zonedDateTime: SExprEncoder[ZonedDateTime]   = stringify(serializers.toString)
  implicit val zoneId: SExprEncoder[ZoneId]                 = stringify(serializers.toString)
  implicit val zoneOffset: SExprEncoder[ZoneOffset]         = stringify(serializers.toString)

  implicit val uuid: SExprEncoder[UUID] = stringify(_.toString)
}
