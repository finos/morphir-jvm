package morphir.ir.qname

/** Generated based on IR.QName
*/
object Codec{

  implicit val encodeQName: io.circe.Encoder[morphir.ir.QName.QName] = ((qName: morphir.ir.QName.QName) =>
    qName match {
      case morphir.ir.QName.QName(arg1, arg2) => 
        io.circe.Json.arr(
          io.circe.Json.fromString("QName"),
          morphir.ir.path.Codec.encodePath(arg1),
          morphir.ir.name.Codec.encodeName(arg2)
        )
    })
  
  implicit val decodeQName: io.circe.Decoder[morphir.ir.QName.QName] = ((c: io.circe.HCursor) =>
    c.withFocus(_.withString(((str) =>
      io.circe.Json.arr(io.circe.Json.fromString(str))))).downN(0).as(morphir.sdk.string.Codec.decodeString).flatMap(((tag) =>
      tag match {
        case "QName" => 
          for {
            arg1 <- c.downN(1).as(morphir.ir.path.Codec.decodePath)
            arg2 <- c.downN(2).as(morphir.ir.name.Codec.decodeName)
          }  yield morphir.ir.QName.QName(
            arg1,
            arg2
          )
      })))

}