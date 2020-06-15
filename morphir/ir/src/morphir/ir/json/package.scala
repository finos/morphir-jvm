package morphir.ir

package object json {
  type Value = Encode.Value
  val JsonEncode: Encode.type = Encode
  val JsonDecode: Decode.type = Decode
}
