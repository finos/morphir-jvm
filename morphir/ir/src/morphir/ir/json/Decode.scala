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

}
