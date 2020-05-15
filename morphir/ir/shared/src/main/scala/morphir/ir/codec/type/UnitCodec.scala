package morphir.ir.codec.`type`
import io.circe.{ Decoder, Encoder }
import morphir.ir.Type.Unit

private[ir] trait UnitCodec {
  implicit def encodeUnit[A](implicit attributesEncoder: Encoder[A]): Encoder[Unit[A]] =
    Encoder.encodeTuple2[String, A].contramap(v => v.tag -> v.attributes)

  implicit def decodeUnit[A](implicit attributesDecoder: Decoder[A]): Decoder[Unit[A]] =
    Decoder.decodeTuple2[String, A].map { case (_, attributes) => Unit(attributes) }

}
