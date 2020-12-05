/*
Copyright 2020 Morgan Stanley

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

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
