package morphir.ir.json

import org.scalactic.{ Bad, Every, Good, One, Or }
import upickle.default._
import morphir.ir.json.Decode.{ DecodeError, Decoder }

import scala.util.control.NonFatal

trait Decode {
  def decodeString[A](input: String)(implicit decoder: Decoder[A]): Decode.DecodeResult[A] =
    try {
      val result = read[A](input)
      Good(result)
    } catch {
      case e: DecodeError.UnexpectedTag => Bad(One(e))
      case e: DecodeError.Failure       => Bad(One(e))
      case NonFatal(t)                  => Bad(One(DecodeError.Failure(t)))
    }
}

object Decode extends Decode {

  type DecodeResult[A] = A Or Every[DecodeError]

  type Value      = ujson.Value
  type Decoder[A] = Reader[A]

  sealed abstract class DecodeError(message: String, cause: Option[Throwable] = None)
      extends Exception(message, cause.orNull)
  object DecodeError {

    def unexpectedTag(actualTag: String, expectedExamples: List[String]): UnexpectedTag =
      new UnexpectedTag(actualTag, expectedExamples)

    def unexpectedTag(actualTag: String, expectedExamples: List[String], message: String): UnexpectedTag =
      UnexpectedTag(actualTag, expectedExamples, message)

    def unexpectedTag(actualTag: String, expectedExample: String, expectedExamples: String*): UnexpectedTag =
      new UnexpectedTag(actualTag, List(expectedExample) ++ expectedExamples)

    final case class Failure(cause: Throwable) extends DecodeError(cause.getMessage, Option(cause))

    final case class UnexpectedTag(actualTag: String, expectedExamples: List[String], message: String)
        extends DecodeError(message) {
      def this(actualTag: String, expectedExamples: List[String]) = {
        this(
          actualTag,
          expectedExamples,
          s"""An unexpected tag was encountered while decoding. The tag: "$actualTag" was not expected, examples of expected tags include: ${expectedExamples
            .mkString("[", ",", "]")}."""
        )
      }
    }
  }
}
