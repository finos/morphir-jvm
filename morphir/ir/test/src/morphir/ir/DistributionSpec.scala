package morphir.ir

import io.circe.{ Decoder, Json, parser }
import io.circe.Decoder.Result
import morphir.ir.Distribution.Distribution
import morphir.ir.distribution.Codec
import zio.test.Assertion.equalTo
import zio.test._

import scala.io.{ BufferedSource, Source }

object DistributionSpec extends DefaultRunnableSpec {
  val distributionDecoder: Decoder[Distribution] = (c: io.circe.HCursor) =>
    c.downField("distribution").as(Codec.decodeDistribution)
  val bufferResource: BufferedSource = Source.fromFile(
    "C:\\Users\\Administrator\\IdeaProjects\\morphir-elm\\tests-integration\\reference-model\\morphir-ir.json"
  )
  val irContent: String = bufferResource.mkString
  bufferResource.close()

  override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] = suite("DistributionSpec")(
    suite("Comparism of Generated IR Json vs Encoded Distribution to IR Json")(
      test("Decode Distribution from IRJson + Re-Encode Distribution + Compare ") {
        val irJson                                   = parser.parse(irContent).getOrElse(Json.Null)
        val distributionResult: Result[Distribution] = distributionDecoder.decodeJson(irJson)
        distributionResult match {
          case Left(_) => assert(true)(equalTo(false)) // cant find an intentional fail
          case Right(distribution) =>
            irJson.hcursor.downField("distribution").as[Json] match {
              case Left(_) => assert(true)(equalTo(false)) // cant find an intentional fail
              case Right(originalDistroFromFile) =>
                Codec.encodeDistribution(distribution).as[Json] match {
                  case Left(_) => assert(true)(equalTo(false)) // cant find an intentional fail
                  case Right(expected) =>
                    assert(originalDistroFromFile)(equalTo(expected))
                }
            }
        }
      }
    )
  )
}
