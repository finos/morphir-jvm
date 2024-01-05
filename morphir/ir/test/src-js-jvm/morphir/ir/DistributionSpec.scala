package morphir.ir

import io.circe.Json
import io.circe.parser.parse
import morphir.ir.formatversion.Codec._
import scala.io.Source
import morphir.testing.MorphirBaseSpec
import zio.test._
object DistributionSpec extends MorphirBaseSpec {

  val irJson = Source.fromFile("morphir/ir/test/resources/morphir/ir/morphir-ir.json")

  def spec = suite("DistributionSpec") {
    test("Decoding and Encoding an IR JSON should match the original decoded JSON") {
      val parsedIr  = parse(irJson.mkString("")).getOrElse(Json.Null)
      val decodedIr = decodeVersionedDistribution(parsedIr.hcursor)

      val encodeIr      = decodedIr.map(encodeVersionedDistribution.apply)
      val encodedIrJson = encodeIr.getOrElse(Json.Null)

      val parsedJson    = parse(encodedIrJson.toString())
      val decodedResult = decodeVersionedDistribution.decodeJson(parsedJson.getOrElse(Json.Null))

      assertTrue(decodedResult == decodedIr)
    }
  }

}
