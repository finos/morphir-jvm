package morphir

import io.circe.Decoder
import morphir.ir.Distribution.Distribution
import morphir.ir.distribution.Codec.{ decodeDistribution, encodeDistribution }

object FormatVersionCodecs {
  implicit val encodeDistributionVersion: io.circe.Encoder[morphir.ir.Distribution.Distribution] =
    (distribution: morphir.ir.Distribution.Distribution) =>
      io.circe.Json.obj(
        ("formatVersion", io.circe.Json.fromInt(FormatVersion.formatVersion)),
        ("distribution", encodeDistribution(distribution))
      )

  implicit val decodeDistributionVersion: Decoder[Distribution] =
    (c: io.circe.HCursor) =>
      c.downField("formatVersion").as[Int].flatMap {
        case FormatVersion.formatVersion => c.downField("distribution").as(decodeDistribution)
        case version =>
          Left(
            io.circe.DecodingFailure(
              s"IR is using an old version is old, please re generate IR. Expected version ${FormatVersion.formatVersion}, but found version $version",
              Nil
            )
          )
      }
}
