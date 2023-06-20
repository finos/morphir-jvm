package morphir.ir.path

/** Generated based on IR.Path
*/
object Codec{

  implicit val encodePath: io.circe.Encoder[morphir.ir.Path.Path] = morphir.sdk.list.Codec.encodeList(morphir.ir.name.Codec.encodeName)
  
  implicit val decodePath: io.circe.Decoder[morphir.ir.Path.Path] = morphir.sdk.list.Codec.decodeList(morphir.ir.name.Codec.decodeName)

}