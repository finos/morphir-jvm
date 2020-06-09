package morphir.ir.json

import cats.data.ValidatedNel
import io.circe
import io.circe.parser
import morphir.ir.json.Decode.Decoder

trait Decode {
  def decodeString[A](input: String)(implicit decoder: Decoder[A]): ValidatedNel[circe.Error, A] =
    parser.decodeAccumulating(input)
}

object Decode extends Decode {

  type Value      = Encode.Value
  type Decoder[A] = io.circe.Decoder[A]

  sealed trait DecodeError extends Exception
  object DecodeError {

    def unexpectedTag(actualTag: String, expectedExamples: List[String]): UnexpectedTag =
      new UnexpectedTag(actualTag, expectedExamples)

    def unexpectedTag(actualTag: String, expectedExamples: List[String], message: String): UnexpectedTag =
      UnexpectedTag(actualTag, expectedExamples, message)

    def unexpectedTag(actualTag: String, expectedExample: String, expectedExamples: String*): UnexpectedTag =
      new UnexpectedTag(actualTag, List(expectedExample) ++ expectedExamples)

    final case class UnexpectedTag(actualTag: String, expectedExamples: List[String], message: String)
        extends Exception(message) {
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
