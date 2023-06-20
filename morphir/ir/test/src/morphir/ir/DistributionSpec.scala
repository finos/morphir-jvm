package morphir.ir

import io.circe.Json
import io.circe.parser.parse
import morphir.FormatVersionCodecs._
import org.scalatest.funsuite.AnyFunSuite
import scala.io.Source

class DistributionSpec extends AnyFunSuite {

  val irJson = Source.fromFile("morphir/ir/test/resources/morphir/ir/morphir-ir.json")

  test("Decoding and Encoding an IR JSON should match the original decoded JSON") {
    val parsedIr  = parse(irJson.mkString("")).getOrElse(Json.Null)
    val decodedIr = decodeDistributionVersion(parsedIr.hcursor)

    val encodeIr      = decodedIr.map(encodeDistributionVersion.apply)
    val encodedIrJson = encodeIr.getOrElse(Json.Null)

    val parsedJson    = parse(encodedIrJson.toString())
    val decodedResult = decodeDistributionVersion.decodeJson(parsedJson.getOrElse(Json.Null))

    assert(decodedResult === decodedIr)
  }
}
