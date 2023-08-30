package morphir.ir

import io.circe.Json
import io.circe.parser.parse
import org.scalatest.funsuite.AnyFunSuite
import morphir.ir.formatversion.Codec._
import scala.io.Source

class DistributionSpec extends AnyFunSuite {

  val irJson = Source.fromFile("morphir/ir/test/resources/morphir/ir/morphir-ir.json")

  test("Decoding and Encoding an IR JSON should match the original decoded JSON") {
    val parsedIr  = parse(irJson.mkString("")).getOrElse(Json.Null)
    val decodedIr = decodeVersionedDistribution(parsedIr.hcursor)

    val encodeIr      = decodedIr.map(encodeVersionedDistribution.apply)
    val encodedIrJson = encodeIr.getOrElse(Json.Null)

    val parsedJson    = parse(encodedIrJson.toString())
    val decodedResult = decodeVersionedDistribution.decodeJson(parsedJson.getOrElse(Json.Null))

    assert(decodedResult === decodedIr)
  }
}
