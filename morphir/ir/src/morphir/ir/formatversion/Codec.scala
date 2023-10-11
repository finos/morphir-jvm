package morphir.ir.formatversion

/** Generated based on IR.FormatVersion
*/
object Codec{

  implicit val encodeVersionedDistribution: io.circe.Encoder[morphir.ir.FormatVersion.VersionedDistribution] = ((versionedDistribution: morphir.ir.FormatVersion.VersionedDistribution) =>
    io.circe.Json.obj(
      ("""formatVersion""", morphir.sdk.basics.Codec.encodeInt(versionedDistribution.formatVersion)),
      ("""distribution""", morphir.ir.distribution.Codec.encodeDistribution(versionedDistribution.distribution))
    ))
  
  implicit val decodeVersionedDistribution: io.circe.Decoder[morphir.ir.FormatVersion.VersionedDistribution] = ((c: io.circe.HCursor) =>
    for {
      formatVersion_ <- c.downField("""formatVersion""").as(morphir.sdk.basics.Codec.decodeInt)
      distribution_ <- c.downField("""distribution""").as(morphir.ir.distribution.Codec.decodeDistribution)
    }  yield morphir.ir.FormatVersion.VersionedDistribution(
      formatVersion_,
      distribution_
    ))

}