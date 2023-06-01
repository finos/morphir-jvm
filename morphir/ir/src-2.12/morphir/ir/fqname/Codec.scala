package morphir.ir.fqname

/** Generated based on IR.FQName
*/
object Codec{

  implicit val encodeFQName: io.circe.Encoder[morphir.ir.FQName.FQName] = ((fQName: (morphir.ir.Path.Path, morphir.ir.Path.Path, morphir.ir.Name.Name)) =>
    io.circe.Json.arr(
      morphir.ir.path.Codec.encodePath(fQName._1),
      morphir.ir.path.Codec.encodePath(fQName._2),
      morphir.ir.name.Codec.encodeName(fQName._3)
    ))
  
  implicit val decodeFQName: io.circe.Decoder[morphir.ir.FQName.FQName] = ((c: io.circe.HCursor) =>
    for {
      arg1 <- morphir.ir.path.Codec.decodePath(c)
      arg2 <- morphir.ir.path.Codec.decodePath(c)
      arg3 <- morphir.ir.name.Codec.decodeName(c)
    }  yield (arg1, arg2, arg3))

}