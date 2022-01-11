package zio.morphir.sexpr

import zio.morphir.sexpr.ast.SExpr
import zio.morphir.sexpr.internal._

import scala.util.control.NoStackTrace

trait SExprDecoder[A] {
  self =>

  final def decodeSExpr(str: CharSequence): Either[String, A] =
    try Right(unsafeDecode(Nil, new FastStringReader(str)))
    catch {
      case SExprDecoder.UnsafeSExpr(trace) => Left(SExprError.render(trace))
      case _: UnexpectedEnd                =>
        Left("Unexpected end of input")
      case _: StackOverflowError           =>
        Left("Unexpected structure")
    }

  /**
   * Returns a new decoder whose decoded values will be mapped by the specified function.
   */
  def map[B](f: A => B): SExprDecoder[B] =
    new SExprDecoder[B] {

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

    def unsafeDecode(trace: List[SExprError], in: RetractReader): B =
      f(self.unsafeDecode(trace, in)) match {
        case Left(err) =>
          throw SExprDecoder.UnsafeSExpr(SExprError.Message(err) :: trace)
        case Right(b)  => b
      }

    override final def fromAST(sexpr: SExpr): Either[String, B] =
      self.fromAST(sexpr).flatMap(f)
  }

  /**
   * Low-level, unsafe method to decode a value or throw an exception. This method should not be
   * called in application code, although it can be implemented for user-defined data structures.
   */
  def unsafeDecode(trace: List[SExprError], in: RetractReader): A

  /**
   * Returns this decoder but widened to the its given super-type
   */
  final def widen[B >: A]: SExprDecoder[B] = self.asInstanceOf[SExprDecoder[B]]

  /**
   * Decode a value from an already parsed SExpr AST.
   *
   * The default implementation encodes the Json to a byte stream and uses decode to parse that.
   * Override to provide a more performant implementation.
   */
  def fromAST(sexpr: SExpr): Either[String, A] =
    decodeSExpr(SExpr.encoder.encodeSExpr(sexpr, None))

}

object SExprDecoder {
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

    def unsafeDecode(trace: List[SExprError], in: RetractReader): String =
      Lexer.string(trace, in).toString

    override final def fromAST(sexpr: SExpr): Either[String, String] =
      sexpr match {
        case SExpr.Str(value) => Right(value)
        case _                => Left("Not a string value")
      }
  }

  implicit val boolean: SExprDecoder[Boolean] = new SExprDecoder[Boolean] {

    def unsafeDecode(trace: List[SExprError], in: RetractReader): Boolean =
      Lexer.boolean(trace, in)

    override final def fromAST(sexpr: SExpr): Either[String, Boolean] =
      sexpr match {
        case SExpr.Bool(value) => Right(value)
        case _                 => Left("Not a bool value")
      }
  }

  implicit val char: SExprDecoder[Char] = string.mapOrFail {
    case str if str.length == 1 => Right(str(0))
    case _                      => Left("expected one character")
  }
}
