package zio.morphir.sexpr

import zio.{Chunk, NonEmptyChunk}

import scala.annotation._
import zio.morphir.sexpr.ast.SExpr
import zio.morphir.sexpr.internal._

import java.math.{BigDecimal, BigInteger}
import java.util.UUID
import scala.math.{BigDecimal => ScalaBigDecimal, BigInt => ScalaBigInt}
import scala.reflect.ClassTag
import scala.collection.immutable

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
   * This default may be overridden when this value may be missing within a SExpr object and still be encoded.
   */
  @nowarn("msg=is never used")
  def isNothing(a: A): Boolean = false

  @nowarn("msg=is never used")
  def xmap[B](f: A => B, g: B => A): SExprEncoder[B] = contramap(g)

  def unsafeEncode(a: A, indent: Option[Int], out: Write): Unit

  def toAST(a: A): Either[String, SExpr] =
    SExpr.decoder.decodeSExpr(encodeSExpr(a, None))

  /**
   * Returns this encoder but narrowed to the its given sub-type
   */
  final def narrow[B <: A]: SExprEncoder[B] = self.asInstanceOf[SExprEncoder[B]]

}

object SExprEncoder extends GeneratedTupleEncoders with EncoderLowPriority1 {
  def apply[A](implicit encoder: SExprEncoder[A]): SExprEncoder[A] = encoder
  def fromFunction[A](encodeFn: (A, Option[Int], Write) => Unit): SExprEncoder[A] = new SExprEncoder[A] {
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

  implicit def option[A](implicit A: SExprEncoder[A]): SExprEncoder[Option[A]] = new SExprEncoder[Option[A]] {
    def unsafeEncode(oa: Option[A], indent: Option[Int], out: Write): Unit =
      oa match {
        case None    => out.write("nil")
        case Some(a) => A.unsafeEncode(a, indent, out)
      }

    override def isNothing(oa: Option[A]): Boolean =
      oa match {
        case None    => true
        case Some(a) => A.isNothing(a)
      }

    override final def toAST(oa: Option[A]): Either[String, SExpr] =
      oa match {
        case None    => Right(SExpr.Nil)
        case Some(a) => A.toAST(a)
      }
  }

  def bump(indent: Option[Int]): Option[Int] = indent match {
    case None    => None
    case Some(i) => Some(i + 1)
  }

  def pad(indent: Option[Int], out: Write): Unit = indent match {
    case None => ()
    case Some(n) =>
      out.write('\n')
      var i = n
      while (i > 0) {
        out.write("  ")
        i -= 1
      }
  }

  implicit def either[A, B](implicit A: SExprEncoder[A], B: SExprEncoder[B]): SExprEncoder[Either[A, B]] =
    new SExprEncoder[Either[A, B]] {
      def unsafeEncode(eab: Either[A, B], indent: Option[Int], out: Write): Unit = {
        out.write('{')
        if (indent.isDefined) unsafeEncodePadded(eab, indent, out)
        else unsafeEncodeCompact(eab, indent, out)
        out.write('}')
      }

      private[this] def unsafeEncodeCompact(eab: Either[A, B], indent: Option[Int], out: Write): Unit =
        eab match {
          case Left(a) =>
            out.write("\"Left\":")
            A.unsafeEncode(a, indent, out)
          case Right(b) =>
            out.write("\"Right\":")
            B.unsafeEncode(b, indent, out)
        }

      private[this] def unsafeEncodePadded(eab: Either[A, B], indent: Option[Int], out: Write): Unit = {
        val indent_ = bump(indent)
        pad(indent_, out)
        eab match {
          case Left(a) =>
            out.write("\"Left\" : ")
            A.unsafeEncode(a, indent_, out)
          case Right(b) =>
            out.write("\"Right\" : ")
            B.unsafeEncode(b, indent_, out)
        }
        pad(indent, out)
      }

      override final def toAST(eab: Either[A, B]): Either[String, SExpr] =
//        eab match {
//          case Left(a)  => A.toAST(a).map(v => SExpr.Obj(Chunk.single("Left" -> v)))
//          case Right(b) => B.toAST(b).map(v => SExpr.Obj(Chunk.single("Right" -> v)))
//        }
        Right(SExpr.Str("")) // todo remove this when SExpr.Obj is implemented
    }

}

private[sexpr] trait EncoderLowPriority1 extends EncoderLowPriority2 {
  self: SExprEncoder.type =>

  implicit def array[A](implicit A: SExprEncoder[A], classTag: ClassTag[A]): SExprEncoder[Array[A]] =
    new SExprEncoder[Array[A]] {
      def unsafeEncode(as: Array[A], indent: Option[Int], out: Write): Unit =
        if (as.isEmpty) out.write("[]")
        else {
          out.write('[')
          if (indent.isDefined) unsafeEncodePadded(as, indent, out)
          else unsafeEncodeCompact(as, indent, out)
          out.write(']')
        }

      private[this] def unsafeEncodeCompact(as: Array[A], indent: Option[Int], out: Write): Unit = {
        val len = as.length
        var i   = 0
        while (i < len) {
          if (i != 0) out.write(',')
          A.unsafeEncode(as(i), indent, out)
          i += 1
        }
      }

      private[this] def unsafeEncodePadded(as: Array[A], indent: Option[Int], out: Write): Unit = {
        val indent_ = bump(indent)
        pad(indent_, out)
        val len = as.length
        var i   = 0
        while (i < len) {
          if (i != 0) {
            out.write(',')
            pad(indent_, out)
          }
          A.unsafeEncode(as(i), indent_, out)
          i += 1
        }
        pad(indent, out)
      }

      override final def toAST(as: Array[A]): Either[String, SExpr] =
        as.map(A.toAST)
          .foldLeft[Either[String, Chunk[SExpr]]](Right(Chunk.empty)) { (s, i) =>
            s.flatMap(chunk => i.map(item => chunk :+ item))
          }
          .map(SExpr.SVector(_))
    }

  implicit def seq[A: SExprEncoder]: SExprEncoder[Seq[A]] = iterable[A, Seq]

  implicit def chunk[A: SExprEncoder]: SExprEncoder[Chunk[A]] = iterable[A, Chunk]

  implicit def nonEmptyChunk[A: SExprEncoder]: SExprEncoder[NonEmptyChunk[A]] = chunk[A].contramap(_.toChunk)

  implicit def indexedSeq[A: SExprEncoder]: SExprEncoder[IndexedSeq[A]] = iterable[A, IndexedSeq]

  implicit def linearSeq[A: SExprEncoder]: SExprEncoder[immutable.LinearSeq[A]] = iterable[A, immutable.LinearSeq]

  implicit def listSet[A: SExprEncoder]: SExprEncoder[immutable.ListSet[A]] = iterable[A, immutable.ListSet]

  implicit def treeSet[A: SExprEncoder]: SExprEncoder[immutable.TreeSet[A]] = iterable[A, immutable.TreeSet]

  implicit def list[A: SExprEncoder]: SExprEncoder[List[A]] = iterable[A, List]

  implicit def vector[A: SExprEncoder]: SExprEncoder[Vector[A]] = iterable[A, Vector]

  implicit def set[A: SExprEncoder]: SExprEncoder[Set[A]] = iterable[A, Set]

  implicit def hashSet[A: SExprEncoder]: SExprEncoder[immutable.HashSet[A]] = iterable[A, immutable.HashSet]

  implicit def sortedSet[A: Ordering: SExprEncoder]: SExprEncoder[immutable.SortedSet[A]] =
    iterable[A, immutable.SortedSet]
}

private[sexpr] trait EncoderLowPriority2 extends EncoderLowPriority3 {
  self: SExprEncoder.type =>

  implicit def iterable[A, T[X] <: Iterable[X]](implicit A: SExprEncoder[A]): SExprEncoder[T[A]] =
    new SExprEncoder[T[A]] {
      def unsafeEncode(as: T[A], indent: Option[Int], out: Write): Unit =
        if (as.isEmpty) out.write("[]")
        else {
          out.write('[')
          if (indent.isDefined) unsafeEncodePadded(as, indent, out)
          else unsafeEncodeCompact(as, indent, out)
          out.write(']')
        }

      private[this] def unsafeEncodeCompact(as: T[A], indent: Option[Int], out: Write): Unit =
        as.foreach {
          var first = true
          a =>
            if (first) first = false
            else out.write(',')
            A.unsafeEncode(a, indent, out)
        }

      private[this] def unsafeEncodePadded(as: T[A], indent: Option[Int], out: Write): Unit = {
        val indent_ = bump(indent)
        pad(indent_, out)
        as.foreach {
          var first = true
          a =>
            if (first) first = false
            else {
              out.write(',')
              pad(indent_, out)
            }
            A.unsafeEncode(a, indent_, out)
        }
        pad(indent, out)
      }

      override final def toAST(as: T[A]): Either[String, SExpr] =
        as.map(A.toAST)
          .foldLeft[Either[String, Chunk[SExpr]]](Right(Chunk.empty)) { (s, i) =>
            s.flatMap(chunk => i.map(item => chunk :+ item))
          }
          .map(SExpr.SVector(_))
    }
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
