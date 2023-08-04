package morphir.ir.name

/** Generated based on IR.Name
*/
object Codec{

  implicit val encodeName: io.circe.Encoder[morphir.ir.Name.Name] = morphir.sdk.list.Codec.encodeList(morphir.sdk.string.Codec.encodeString)
  
  implicit val decodeName: io.circe.Decoder[morphir.ir.Name.Name] = morphir.sdk.list.Codec.decodeList(morphir.sdk.string.Codec.decodeString)

}