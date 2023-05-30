package morphir.ir

import io.circe.{ Decoder, Json, parser }
import io.circe.Decoder.Result
import morphir.ir.Distribution.Distribution
import morphir.ir.distribution.Codec._
import morphir.FormatVersionCodecs._
import org.scalatest.funsuite.AnyFunSuite
import scala.io.{ BufferedSource, Source }

class DistributionSpec extends AnyFunSuite {
  val distributionDecoder: Decoder[Distribution] = (c: io.circe.HCursor) =>
    c.downField("distribution").as(decodeDistribution)
  val bufferResource: BufferedSource = Source.fromFile(
    "morphir/ir/test/resources/morphir/ir/morphir-ir.json"
  )
  val irContent: String = bufferResource.mkString
  bufferResource.close()

  test("Decoding and Encoding an IR JSON should match the original JSON") {
    val irJson                                   = parser.parse(irContent)
    val distributionResult: Result[Distribution] = decodeDistributionVersion.decodeJson(irJson.getOrElse(Json.Null))
    val encodedDistributionResult                = distributionResult.map(encodeDistributionVersion.apply)
    assert(encodedDistributionResult === irJson)
  }
}
