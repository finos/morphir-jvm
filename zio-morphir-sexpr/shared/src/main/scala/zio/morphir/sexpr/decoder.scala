package zio.morphir.sexpr

import zio.morphir.sexpr.ast.SExpr
import zio.morphir.sexpr.internal._
import zio.morphir.sexpr.javatime.parsers
import zio.morphir.sexpr.uuid.UUIDParser
import zio.{Chunk, NonEmptyChunk}

import java.util.UUID
import scala.annotation._
import scala.collection.immutable.{LinearSeq, ListSet, TreeSet}
import scala.collection.{immutable, mutable}
import scala.util.control.NoStackTrace

trait SExprDecoder[A] { self =>

  /**
   * An alias for [[SExprDecoder#orElse]].
   */
  final def <>[A1 >: A](that: => SExprDecoder[A1]): SExprDecoder[A1] = self.orElse(that)

  /**
   * An alias for [[SExprDecoder#orElseEither]].
   */
  final def <+>[B](that: => SExprDecoder[B]): SExprDecoder[Either[A, B]] = self.orElseEither(that)

  /**
   * An alias for [[SExprDecoder#zip]].
   */
  final def <*>[B](that: => SExprDecoder[B]): SExprDecoder[(A, B)] = self.zip(that)

  /**
   * An alias for [[SExprDecoder#zipRight]].
   */
  final def *>[B](that: => SExprDecoder[B]): SExprDecoder[B] = self.zipRight(that)

  /**
   * An alias for [[SExprDecoder#zipLeft]].
   */
  final def <*[B](that: => SExprDecoder[B]): SExprDecoder[A] = self.zipLeft(that)

  /**
   * Attempts to decode a value of type `A` from the specified `CharSequence`, but may fail with
   * a human-readable error message if the provided text does not encode a value of this type.
   *
   * Note: This method may not entirely consume the specified character sequence.
   */

  final def decodeSExpr(str: CharSequence): Either[String, A] =
    try Right(unsafeDecode(Nil, new FastStringReader(str)))
    catch {
      case SExprDecoder.UnsafeSExpr(trace) => Left(SExprError.render(trace))
      case _: UnexpectedEnd                => Left("Unexpected end of input")
      case _: StackOverflowError           => Left("Unexpected structure")
    }

  /**
   * Decode a value from an already parsed SExpr AST.
   *
   * The default implementation encodes the SExpr to a byte stream and uses decode to parse that.
   * Override to provide a more performant implementation.
   */
  def fromAST(sexpr: SExpr): Either[String, A] = decodeSExpr(SExpr.encoder.encodeSExpr(sexpr, None))

  /**
   * Returns a new decoder whose decoded values will be mapped by the specified function.
   */
  def map[B](f: A => B): SExprDecoder[B] = new SExprDecoder[B] {
    def unsafeDecode(trace: List[SExprError], in: RetractReader): B =
      f(self.unsafeDecode(trace, in))

    override final def fromAST(sexpr: SExpr): Either[String, B] =
      self.fromAST(sexpr).map(f)
  }

  /**
   * Returns a new codec whose decoded values will be mapped by the specified function, which may
   * itself decide to fail with some type of error.
   */
  final def mapOrFail[B](f: A => Either[String, B]): SExprDecoder[B] = new SExprDecoder[B] {
    def unsafeDecode(trace: List[SExprError], in: RetractReader): B = f(self.unsafeDecode(trace, in)) match {
      case Left(err) =>
        throw SExprDecoder.UnsafeSExpr(SExprError.Message(err) :: trace)
      case Right(b)  => b
    }

    override final def fromAST(sexpr: SExpr): Either[String, B] = self.fromAST(sexpr).flatMap(f)
  }

  /**
   * Returns a new codec that combines this codec and the specified codec using fallback semantics:
   * such that if this codec fails, the specified codec will be tried instead.
   * This method may be unsafe from a security perspective: it can use more memory than hand coded
   * alternative and so lead to DOS.
   *
   * For example, in the case of an alternative between `Int` and `Boolean`, a hand coded
   * alternative would look like:
   *
   * ```
   * val decoder: SExprDecoder[AnyVal] = SExprDecoder.peekChar[AnyVal] {
   * case 't' | 'f' => SExprDecoder[Boolean].widen
   * case c         => SExprDecoder[Int].widen
   * }
   * ```
   */
  final def orElse[A1 >: A](that: => SExprDecoder[A1]): SExprDecoder[A1] = new SExprDecoder[A1] {
    def unsafeDecode(trace: List[SExprError], in: RetractReader): A1 = {
      val in2 = new zio.morphir.sexpr.internal.WithRecordingReader(in, 64)

      try self.unsafeDecode(trace, in2)
      catch {
        case SExprDecoder.UnsafeSExpr(_) =>
          in2.rewind()
          that.unsafeDecode(trace, in2)

        case _: UnexpectedEnd =>
          in2.rewind()
          that.unsafeDecode(trace, in2)
      }
    }

    override final def fromAST(sexpr: SExpr): Either[String, A1] =
      self.fromAST(sexpr) match {
        case Left(_)           => that.fromAST(sexpr)
        case result @ Right(_) => result
      }
  }

  /**
   * Returns a new codec that combines this codec and the specified codec using fallback semantics:
   * such that if this codec fails, the specified codec will be tried instead.
   */
  final def orElseEither[B](that: => SExprDecoder[B]): SExprDecoder[Either[A, B]] =
    self.map(Left(_)).orElse(that.map(Right(_)))

  /**
   * Low-level, unsafe method to decode a value or throw an exception. This method should not be
   * called in application code, although it can be implemented for user-defined data structures.
   */
  def unsafeDecode(trace: List[SExprError], in: RetractReader): A

  def unsafeDecodeMissing(trace: List[SExprError]): A =
    throw SExprDecoder.UnsafeSExpr(SExprError.Message("missing") :: trace)

  /**
   * Returns this decoder but widened to the its given super-type
   */
  final def widen[B >: A]: SExprDecoder[B] = self.asInstanceOf[SExprDecoder[B]]

  /**
   * Returns a new codec that combines this codec and the specified codec into a single codec that
   * decodes a tuple of the values decoded by the respective codecs.
   */
  final def zip[B](that: => SExprDecoder[B]): SExprDecoder[(A, B)] = SExprDecoder.tuple2(this, that)

  /**
   * Zips two codecs, but discards the output on the right hand side.
   */
  final def zipLeft[B](that: => SExprDecoder[B]): SExprDecoder[A] = self.zipWith(that)((a, _) => a)

  /**
   * Zips two codecs, but discards the output on the left hand side.
   */
  final def zipRight[B](that: => SExprDecoder[B]): SExprDecoder[B] = self.zipWith(that)((_, b) => b)

  /**
   * Zips two codecs into one, transforming the outputs of both codecs by the specified function.
   */
  final def zipWith[B, C](that: => SExprDecoder[B])(f: (A, B) => C): SExprDecoder[C] = self.zip(that).map(f.tupled)
}

object SExprDecoder extends GeneratedTupleDecoders with DecoderLowPriority1 {
  type SExprError = zio.morphir.sexpr.SExprError
  val SExprError = zio.morphir.sexpr.SExprError

  def apply[A](implicit decoder: SExprDecoder[A]): SExprDecoder[A] = decoder

  final case class UnsafeSExpr(trace: List[SExprError])
      extends Exception("If you see this, a developer made a mistake using SExprDecoder")
      with NoStackTrace

  def peekChar[A](partialFunction: PartialFunction[Char, SExprDecoder[A]]): SExprDecoder[A] = new SExprDecoder[A] {
    override def unsafeDecode(trace: List[SExprError], in: RetractReader): A = {
      val c = in.nextNonWhitespace()
      if (partialFunction.isDefinedAt(c)) {
        in.retract()
        partialFunction(c).unsafeDecode(trace, in)
      } else {
        throw UnsafeSExpr(SExprError.Message(s"missing case in `peekChar` for '${c}''") :: trace)
      }
    }
  }

  implicit val string: SExprDecoder[String] = new SExprDecoder[String] {
    def unsafeDecode(trace: List[SExprError], in: RetractReader): String = Lexer.string(trace, in).toString

    override final def fromAST(sexpr: SExpr): Either[String, String] = sexpr match {
      case SExpr.Str(value) => Right(value)
      case _                => Left("Not a string value")
    }
  }

  implicit val boolean: SExprDecoder[Boolean] = new SExprDecoder[Boolean] {
    def unsafeDecode(trace: List[SExprError], in: RetractReader): Boolean = Lexer.boolean(trace, in)

    override final def fromAST(sexpr: SExpr): Either[String, Boolean] = sexpr match {
      case SExpr.Bool(value) => Right(value)
      case _                 => Left("Not a bool value")
    }
  }

  //TODO: We may want to support Clojure style Character literals instead
  implicit val char: SExprDecoder[Char]                       = string.mapOrFail {
    case str if str.length == 1 => Right(str(0))
    case _                      => Left("expected one character")
  }
  implicit val symbol: SExprDecoder[Symbol]                   = string.map(Symbol(_))
  implicit val byte: SExprDecoder[Byte]                       = number(Lexer.byte, _.byteValueExact())
  implicit val short: SExprDecoder[Short]                     = number(Lexer.short, _.shortValueExact())
  implicit val int: SExprDecoder[Int]                         = number(Lexer.int, _.intValueExact())
  implicit val long: SExprDecoder[Long]                       = number(Lexer.long, _.longValueExact())
  implicit val bigInteger: SExprDecoder[java.math.BigInteger] = number(Lexer.bigInteger, _.toBigIntegerExact)
  implicit val scalaBigInt: SExprDecoder[BigInt]              = bigInteger.map(x => x)
  implicit val float: SExprDecoder[Float]                     = number(Lexer.float, _.floatValue())
  implicit val double: SExprDecoder[Double]                   = number(Lexer.double, _.doubleValue())
  implicit val bigDecimal: SExprDecoder[java.math.BigDecimal] = number(Lexer.bigDecimal, identity)
  implicit val scalaBigDecimal: SExprDecoder[BigDecimal]      = bigDecimal.map(x => x)

  // numbers decode from numbers or strings for maximum compatibility
  private[this] def number[A](
    f: (List[SExprError], RetractReader) => A,
    fromBigDecimal: java.math.BigDecimal => A
  ): SExprDecoder[A]                                                           =
    new SExprDecoder[A] {
      def unsafeDecode(trace: List[SExprError], in: RetractReader): A = (in.nextNonWhitespace(): @switch) match {
        case '"' =>
          val i = f(trace, in)
          Lexer.charOnly(trace, in, '"')
          i
        case _   =>
          in.retract()
          f(trace, in)
      }

      override final def fromAST(sexpr: SExpr): Either[String, A] = sexpr match {
        case SExpr.Num(value) =>
          try Right(fromBigDecimal(value))
          catch {
            case exception: ArithmeticException => Left(exception.getMessage)
          }
        case SExpr.Str(value) =>
          val reader = new FastStringReader(value)
          val result =
            try Right(f(List.empty, reader))
            catch {
              case SExprDecoder.UnsafeSExpr(trace) => Left(SExprError.render(trace))
              case _: UnexpectedEnd                => Left("Unexpected end of input")
              case _: StackOverflowError           => Left("Unexpected structure")
            } finally reader.close()
          result
        case _                => Left("Not a number or a string")
      }
    }
  // Option treats empty and null values as Nothing and passes values to the decoder.
  //
  // If alternative behaviour is desired, e.g. pass null to the underlying, then
  // use a newtype wrapper.
  implicit def option[A](implicit A: SExprDecoder[A]): SExprDecoder[Option[A]] = new SExprDecoder[Option[A]] { self =>
    private[this] val ull: Array[Char] = "ull".toCharArray

    override def unsafeDecodeMissing(trace: List[SExprError]): Option[A] = Option.empty

    def unsafeDecode(trace: List[SExprError], in: RetractReader): Option[A] =
      (in.nextNonWhitespace(): @switch) match {
        case 'n' =>
          Lexer.readChars(trace, in, ull, "null")
          None
        case _   =>
          in.retract()
          Some(A.unsafeDecode(trace, in))
      }

    // overridden here to pass `None` to the new Decoder instead of throwing
    // when called from a derived decoder
    override def map[B](f: Option[A] => B): SExprDecoder[B] = new SExprDecoder[B] {
      override def unsafeDecodeMissing(trace: List[SExprError]): B =
        f(None)

      def unsafeDecode(trace: List[SExprError], in: RetractReader): B =
        f(self.unsafeDecode(trace, in))

      override final def fromAST(sexpr: SExpr): Either[String, B] =
        self.fromAST(sexpr).map(f)
    }

    override final def fromAST(sexpr: SExpr): Either[String, Option[A]] = sexpr match {
      //case SExpr.Null => Right(None)    // TODO need to decide what to do for null
      case _ => A.fromAST(sexpr).map(Some.apply)
    }
  }
  /*  TO DO need to figure out how to do Either
  // supports multiple representations for compatibility with other libraries,
  // but does not support the "discriminator field" encoding with a field named
  // "value" used by some libraries.
  implicit def either[A, B](implicit
    A: SExprDecoder[A],
    B: SExprDecoder[B]
  ): SExprDecoder[Either[A, B]] =
    new SExprDecoder[Either[A, B]] {

      val names: Array[String] =
        Array("a", "Left", "left", "b", "Right", "right")
      val matrix: StringMatrix    = new StringMatrix(names)
      val spans: Array[SExprError] = names.map(SExprError.ObjectAccess)

      def unsafeDecode(
        trace: List[SExprError],
        in: RetractReader
      ): Either[A, B] = {
        Lexer.char(trace, in, '{')

        val values: Array[Any] = Array.ofDim(2)

        if (Lexer.firstField(trace, in))
          while ({
            {
              val field = Lexer.field(trace, in, matrix)
              if (field == -1) Lexer.skipValue(trace, in)
              else {
                val trace_ = spans(field) :: trace
                if (field < 3) {
                  if (values(0) != null)
                    throw UnsafeSExpr(SExprError.Message("duplicate") :: trace_)
                  values(0) = A.unsafeDecode(trace_, in)
                } else {
                  if (values(1) != null)
                    throw UnsafeSExpr(SExprError.Message("duplicate") :: trace_)
                  values(1) = B.unsafeDecode(trace_, in)
                }
              }
            }; Lexer.nextField(trace, in)
          }) ()

        if (values(0) == null && values(1) == null)
          throw UnsafeSExpr(SExprError.Message("missing fields") :: trace)
        if (values(0) != null && values(1) != null)
          throw UnsafeSExpr(
            SExprError.Message("ambiguous either, both present") :: trace
          )
        if (values(0) != null)
          Left(values(0).asInstanceOf[A])
        else Right(values(1).asInstanceOf[B])
      }
    }
   */
  private[sexpr] def builder[A, T[_]](trace: List[SExprError], in: RetractReader, builder: mutable.Builder[A, T[A]])(
    implicit A: SExprDecoder[A]
  ): T[A] = {
    Lexer.char(trace, in, '[')
    var i: Int = 0
    if (Lexer.firstArrayElement(in)) while ({
      {
        val trace_ = SExprError.IndexedAccess(i) :: trace
        builder += A.unsafeDecode(trace_, in)
        i += 1
      }; Lexer.nextArrayElement(trace, in)
    }) ()
    builder.result()
  }

  // use this instead of `string.mapOrFail` in supertypes (to prevent class initialization error at runtime)
  private[sexpr] def mapStringOrFail[A](f: String => Either[String, A]): SExprDecoder[A] = new SExprDecoder[A] {
    def unsafeDecode(trace: List[SExprError], in: RetractReader): A =
      f(string.unsafeDecode(trace, in)) match {
        case Left(err)    => throw UnsafeSExpr(SExprError.Message(err) :: trace)
        case Right(value) => value
      }

    override def fromAST(sexpr: SExpr): Either[String, A] =
      string.fromAST(sexpr).flatMap(f)
  }
}

private[sexpr] trait DecoderLowPriority1 extends DecoderLowPriority2 { this: SExprDecoder.type =>
  implicit def array[A: SExprDecoder: reflect.ClassTag]: SExprDecoder[Array[A]] = new SExprDecoder[Array[A]] {
    def unsafeDecode(trace: List[SExprError], in: RetractReader): Array[A] =
      builder(trace, in, Array.newBuilder[A])
  }

  implicit def seq[A: SExprDecoder]: SExprDecoder[Seq[A]] = new SExprDecoder[Seq[A]] {
    def unsafeDecode(trace: List[SExprError], in: RetractReader): Seq[A] =
      builder(trace, in, immutable.Seq.newBuilder[A])
  }

  implicit def chunk[A: SExprDecoder]: SExprDecoder[Chunk[A]] = new SExprDecoder[Chunk[A]] {
    def unsafeDecode(trace: List[SExprError], in: RetractReader): Chunk[A] =
      builder(trace, in, zio.ChunkBuilder.make[A]())

    override final def fromAST(sexpr: SExpr): Either[String, Chunk[A]] =
      sexpr match {
        case SExpr.SVector(elements) =>
          elements.foldLeft[Either[String, Chunk[A]]](Right(Chunk.empty)) { (s, item) =>
            s.flatMap(chunk =>
              implicitly[SExprDecoder[A]].fromAST(item).map { a =>
                chunk :+ a
              }
            )
          }
        case _                       => Left("Not an array")
      }
  }

  implicit def nonEmptyChunk[A: SExprDecoder]: SExprDecoder[NonEmptyChunk[A]] =
    chunk[A].mapOrFail(NonEmptyChunk.fromChunk(_).toRight("Chunk was empty"))

  implicit def indexedSeq[A: SExprDecoder]: SExprDecoder[IndexedSeq[A]] = new SExprDecoder[IndexedSeq[A]] {
    def unsafeDecode(trace: List[SExprError], in: RetractReader): IndexedSeq[A] =
      builder(trace, in, IndexedSeq.newBuilder[A])
  }

  implicit def linearSeq[A: SExprDecoder]: SExprDecoder[immutable.LinearSeq[A]] =
    new SExprDecoder[immutable.LinearSeq[A]] {
      def unsafeDecode(trace: List[SExprError], in: RetractReader): LinearSeq[A] =
        builder(trace, in, immutable.LinearSeq.newBuilder[A])
    }

  implicit def listSet[A: SExprDecoder]: SExprDecoder[immutable.ListSet[A]] = new SExprDecoder[immutable.ListSet[A]] {
    def unsafeDecode(trace: List[SExprError], in: RetractReader): ListSet[A] =
      builder(trace, in, immutable.ListSet.newBuilder[A])
  }

  implicit def treeSet[A: SExprDecoder: Ordering]: SExprDecoder[immutable.TreeSet[A]] =
    new SExprDecoder[immutable.TreeSet[A]] {
      def unsafeDecode(trace: List[SExprError], in: RetractReader): TreeSet[A] =
        builder(trace, in, immutable.TreeSet.newBuilder[A])
    }

  implicit def list[A: SExprDecoder]: SExprDecoder[List[A]] = new SExprDecoder[List[A]] {
    def unsafeDecode(trace: List[SExprError], in: RetractReader): List[A] =
      builder(trace, in, new mutable.ListBuffer[A])
  }

  implicit def vector[A: SExprDecoder]: SExprDecoder[Vector[A]] = new SExprDecoder[Vector[A]] {
    def unsafeDecode(trace: List[SExprError], in: RetractReader): Vector[A] =
      builder(trace, in, immutable.Vector.newBuilder[A])
  }

  implicit def set[A: SExprDecoder]: SExprDecoder[Set[A]] = new SExprDecoder[Set[A]] {
    def unsafeDecode(trace: List[SExprError], in: RetractReader): Set[A] =
      builder(trace, in, Set.newBuilder[A])
  }

  implicit def hashSet[A: SExprDecoder]: SExprDecoder[immutable.HashSet[A]] = new SExprDecoder[immutable.HashSet[A]] {
    def unsafeDecode(trace: List[SExprError], in: RetractReader): immutable.HashSet[A] =
      builder(trace, in, immutable.HashSet.newBuilder[A])
  }

  implicit def sortedSet[A: Ordering: SExprDecoder]: SExprDecoder[immutable.SortedSet[A]] =
    new SExprDecoder[immutable.SortedSet[A]] {
      def unsafeDecode(trace: List[SExprError], in: RetractReader): immutable.SortedSet[A] =
        builder(trace, in, immutable.SortedSet.newBuilder[A])
    }

}
// We have a hierarchy of implicits for two reasons:
//
// 1. the compiler searches each scope and returns early if it finds a match.
//    This means that it is faster to put more complex derivation rules (that
//    are unlikely to be commonly used) into a lower priority scope, allowing
//    simple things like primitives to match fast.
//
// 2. sometimes we want to have overlapping instances with a more specific /
//    optimised instances, and a fallback for the more general case that would
//    otherwise conflict in a lower priority scope. A good example of this is to
//    have specialised decoders for collection types, falling back to BuildFrom.
private[sexpr] trait DecoderLowPriority2 extends DecoderLowPriority3 { this: SExprDecoder.type =>
  implicit def iterable[A: SExprDecoder]: SExprDecoder[Iterable[A]] = new SExprDecoder[Iterable[A]] {
    def unsafeDecode(trace: List[SExprError], in: RetractReader): Iterable[A] =
      builder(trace, in, immutable.Iterable.newBuilder[A])
  }
}

private[sexpr] trait DecoderLowPriority3 { this: SExprDecoder.type =>
  import java.time.format.DateTimeParseException
  import java.time.zone.ZoneRulesException
  import java.time._

  implicit val dayOfWeek: SExprDecoder[DayOfWeek] =
    mapStringOrFail(s => parseJavaTime(DayOfWeek.valueOf, s.toUpperCase))
  implicit val duration: SExprDecoder[Duration]   = mapStringOrFail(parseJavaTime(parsers.unsafeParseDuration, _))
  implicit val instant: SExprDecoder[Instant]     = mapStringOrFail(parseJavaTime(parsers.unsafeParseInstant, _))
  implicit val localDate: SExprDecoder[LocalDate] = mapStringOrFail(parseJavaTime(parsers.unsafeParseLocalDate, _))

  implicit val localDateTime: SExprDecoder[LocalDateTime] =
    mapStringOrFail(parseJavaTime(parsers.unsafeParseLocalDateTime, _))

  implicit val localTime: SExprDecoder[LocalTime] = mapStringOrFail(parseJavaTime(parsers.unsafeParseLocalTime, _))
  implicit val month: SExprDecoder[Month]         = mapStringOrFail(s => parseJavaTime(Month.valueOf, s.toUpperCase))
  implicit val monthDay: SExprDecoder[MonthDay]   = mapStringOrFail(parseJavaTime(parsers.unsafeParseMonthDay, _))

  implicit val offsetDateTime: SExprDecoder[OffsetDateTime] =
    mapStringOrFail(parseJavaTime(parsers.unsafeParseOffsetDateTime, _))

  implicit val offsetTime: SExprDecoder[OffsetTime] = mapStringOrFail(parseJavaTime(parsers.unsafeParseOffsetTime, _))
  implicit val period: SExprDecoder[Period]         = mapStringOrFail(parseJavaTime(parsers.unsafeParsePeriod, _))
  implicit val year: SExprDecoder[Year]             = mapStringOrFail(parseJavaTime(parsers.unsafeParseYear, _))
  implicit val yearMonth: SExprDecoder[YearMonth]   = mapStringOrFail(parseJavaTime(parsers.unsafeParseYearMonth, _))

  implicit val zonedDateTime: SExprDecoder[ZonedDateTime] = mapStringOrFail(
    parseJavaTime(parsers.unsafeParseZonedDateTime, _)
  )

  implicit val zoneId: SExprDecoder[ZoneId]         = mapStringOrFail(parseJavaTime(parsers.unsafeParseZoneId, _))
  implicit val zoneOffset: SExprDecoder[ZoneOffset] = mapStringOrFail(parseJavaTime(parsers.unsafeParseZoneOffset, _))

  // Commonized handling for decoding from string to java.time Class
  private[sexpr] def parseJavaTime[A](f: String => A, s: String): Either[String, A] =
    try Right(f(s))
    catch {
      case zre: ZoneRulesException      => Left(s"$s is not a valid ISO-8601 format, ${zre.getMessage}")
      case dtpe: DateTimeParseException => Left(s"$s is not a valid ISO-8601 format, ${dtpe.getMessage}")
      case dte: DateTimeException       => Left(s"$s is not a valid ISO-8601 format, ${dte.getMessage}")
      case ex: Exception                => Left(ex.getMessage)
    }

  implicit val uuid: SExprDecoder[UUID] =
    mapStringOrFail { str =>
      try Right(UUIDParser.unsafeParse(str))
      catch {
        case iae: IllegalArgumentException => Left(s"Invalid UUID: ${iae.getMessage}")
      }
    }
}
