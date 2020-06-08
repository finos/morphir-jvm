package morphir.ir.codec

import io.circe.{ Decoder, Encoder }
import morphir.ir.documented.Documented
import upickle.default._

object documentedCodecs {

  trait DocumentedCodec {

    implicit def documentedReadWriter[A: ReadWriter]: ReadWriter[Documented[A]] =
      readwriter[(String, A)].bimap[Documented[A]](
        instance => (instance.doc, instance.value), {
          case (docs, value) => Documented(docs, value)
        }
      )

    implicit def encodeDocumented[A: Encoder]: Encoder[Documented[A]] =
      Encoder.encodeTuple2[String, A].contramap(_.toTuple)

    implicit def decodeDocumented[A: Decoder]: Decoder[Documented[A]] =
      Decoder.decodeTuple2[String, A].map(Documented.fromTuple)

  }
}
