package morphir.ir.distribution

import morphir.FormatVersion
import morphir.ir.Distribution.Distribution

/**
 * Generated based on IR.Distribution
 */
object Codec {

  val encodeDistribution: io.circe.Encoder[morphir.ir.Distribution.Distribution] = (
    (distribution: morphir.ir.Distribution.Distribution) =>
      distribution match {
        case morphir.ir.Distribution.Library(arg1, arg2, arg3) =>
          io.circe.Json.arr(
            io.circe.Json.fromString("Library"),
            morphir.ir._package.Codec.encodePackageName(arg1),
            morphir.sdk.dict.Codec.encodeDict(
              morphir.ir._package.Codec.encodePackageName,
              morphir.ir._package.Codec.encodeSpecification(morphir.sdk.basics.Codec.encodeUnit)
            )(arg2),
            morphir.ir._package.Codec.encodeDefinition(
              morphir.sdk.basics.Codec.encodeUnit,
              morphir.ir._type.Codec.encodeType(morphir.sdk.basics.Codec.encodeUnit)
            )(arg3)
          )
      }
    )

  implicit val encodeVersionDistribution: io.circe.Encoder[morphir.ir.Distribution.Distribution] =
    (distribution: morphir.ir.Distribution.Distribution) =>
      io.circe.Json.obj(("formatVersion", io.circe.Json.fromInt(FormatVersion.formatVersion)),
        ("distribution", encodeDistribution(distribution)))


  implicit val decodeVersionDistribution: io.circe.Decoder[Distribution] =
    (c: io.circe.HCursor) =>
      c.downField("formatVersion").as[Int]
        .flatMap{
          case FormatVersion.formatVersion => c.downField("distribution").as(decodeDistribution)
          case _ => Left(io.circe.DecodingFailure("IR version is old, please re generate IR",Nil))
        }



   val decodeDistribution: io.circe.Decoder[morphir.ir.Distribution.Distribution] = (
    (c: io.circe.HCursor) =>
      c.withFocus(_.withString(((str) => io.circe.Json.arr(io.circe.Json.fromString(str)))))
        .downN(0)
        .as(morphir.sdk.string.Codec.decodeString)
        .flatMap(
          (
            (tag) =>
              tag match {
                case "Library" =>
                  for {
                    arg1 <- c.downN(1).as(morphir.ir._package.Codec.decodePackageName)
                    arg2 <- c.downN(2)
                      .as(
                        morphir.sdk.dict.Codec.decodeDict(
                          morphir.ir._package.Codec.decodePackageName,
                          morphir.ir._package.Codec.decodeSpecification(morphir.sdk.basics.Codec.decodeUnit)
                        )
                      )
                    arg3 <- c.downN(3)
                      .as(
                        morphir.ir._package.Codec.decodeDefinition(
                          morphir.sdk.basics.Codec.decodeUnit,
                          morphir.ir._type.Codec.decodeType(morphir.sdk.basics.Codec.decodeUnit)
                        )
                      )
                  } yield morphir.ir.Distribution.Library(
                    arg1,
                    arg2,
                    arg3
                  )
              }
            )
        )
    )

}
