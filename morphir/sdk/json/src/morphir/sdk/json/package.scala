package morphir.sdk

package object json {
  type Decoder[A] = Decode.Decoder[A]

  type DecodeResult[A] = Decode.DecodeResult[A]
  val DecodeResult = Decode.DecodeResult
}
