package morphir.ir.testing

import cats.data.Validated.Valid
import cats.data.ValidatedNel
import io.circe.syntax._
import io.circe.parser._
import io.circe._
import morphir.ir.json.JsonFacade
import zio.test._
import zio.test.Assertion._

import scala.reflect.ClassTag

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

  def testEncodesToJSON[T](sut: T, json: String)(
    implicit encoder: Encoder[T],
    tag: ClassTag[T]
  ): ZSpec[Any, Nothing] = {
    val typeName = tag.runtimeClass.getSimpleName
    test(s"Given $typeName: $sut it should encode to: $json") {
      assert(JsonFacade.encode(sut, 0))(equalTo(json))
    }
  }

}
