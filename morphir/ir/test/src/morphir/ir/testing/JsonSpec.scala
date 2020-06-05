package morphir.ir.testing

import cats.data.Validated.Valid
import cats.data.ValidatedNel
import io.circe.syntax._
import io.circe.parser._
import io.circe._
import morphir.ir.json.JsonFacade
import zio.test._
import zio.test.Assertion._
import zio.{ console, ZIO }

trait JsonSpec extends JsonFacade { this: DefaultRunnableSpec =>

  def checkCodecIsWellBehaved[A](
    value: A
  )(implicit encoder: Encoder[A], decoder: Decoder[A]) =
    for {
      encoded <- ZIO.effect(value.asJson.noSpaces)
      decoded <- ZIO.effect(decodeAccumulating(encoded))
      _       <- console.putStrLn(s"JSON: ${value.asJson.spaces2}")
    } yield zio.test.assert(decoded)(equalTo(Valid(value)))

  def assertCodecIsWellBehaved[A](
    value: A
  )(
    assertion: Assertion[ValidatedNel[io.circe.Error, A]]
  )(implicit encoder: Encoder[A], decoder: Decoder[A]): TestResult = {
    val encoded = value.asJson.noSpaces
    val decoded = decodeAccumulating(encoded)
    zio.test.assert(decoded)(assertion)
  }

  def assertEncodesToExpectedCompactJsonString[A](
    value: A
  )(expected: String)(implicit encoder: Encoder[A]): TestResult =
    assert(value.asJson.noSpaces)(equalTo(expected))

  def assertEncodesAsExpected[A](
    value: A
  )(assertion: Assertion[String])(implicit encoder: Encoder[A]): TestResult =
    assert(value.asJson.noSpaces)(assertion)

}
