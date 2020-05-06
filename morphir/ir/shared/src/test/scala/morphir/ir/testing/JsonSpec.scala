package morphir.ir.testing

import cats.data.Validated.Valid
import cats.data.ValidatedNel
import io.circe.syntax._
import io.circe.parser._
import io.circe._
import zio.test._
import zio.test.Assertion._

trait JsonSpec { this: DefaultRunnableSpec =>
  def checkCodecIsWellBehaved[A](
    value: A
  )(implicit encoder: Encoder[A], decoder: Decoder[A]): TestResult = {
    val encoded = value.asJson.noSpaces
    val decoded = decodeAccumulating(encoded)
    zio.test.assert(decoded)(equalTo(Valid(value)))
  }

  def checkCodecIsWellBehaved[A](
    value: A
  )(
    assertion: Assertion[ValidatedNel[io.circe.Error, A]]
  )(implicit encoder: Encoder[A], decoder: Decoder[A]): TestResult = {
    val encoded = value.asJson.noSpaces
    val decoded = decodeAccumulating(encoded)
    zio.test.assert(decoded)(assertion)
  }

}
