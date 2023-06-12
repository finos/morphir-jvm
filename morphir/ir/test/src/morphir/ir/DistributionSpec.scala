package morphir.ir

import io.circe.{ Json, parser }
import io.circe.Decoder.Result
import morphir.FormatVersionCodecs._
import org.scalatest.funsuite.AnyFunSuite
import scala.io.{ BufferedSource, Source }

class DistributionSpec extends AnyFunSuite {
  val bufferResource: BufferedSource = Source.fromFile(
    "morphir/ir/test/resources/morphir/ir/morphir-ir.json"
  )
  val irContent: String = bufferResource.mkString
  bufferResource.close()

  test("Decoding and Encoding an IR JSON should match the original JSON") {
    val irJson = parser.parse(irContent)
    val distributionResult: Result[morphir.ir.Distribution.Distribution] =
      decodeDistributionVersion.decodeJson(irJson.getOrElse(Json.Null))
    val encodedDistributionResult = distributionResult.map(encodeDistributionVersion.apply)
    assert(encodedDistributionResult === irJson)
  }
}
