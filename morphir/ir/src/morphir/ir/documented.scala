package morphir.ir

import io.circe.{ Decoder, Encoder }

object documented {

  case class Documented[+A](doc: String, value: A) {
    @inline def toTuple: (String, A) = doc -> value
  }

  object Documented {

    implicit def encodeDocumented[A: Encoder]: Encoder[Documented[A]] =
      Encoder.encodeTuple2[String, A].contramap(_.toTuple)

    implicit def decodeDocumented[A: Decoder]: Decoder[Documented[A]] =
      Decoder.decodeTuple2[String, A].map(fromTuple)

    def fromTuple[A](tuple: (String, A)): Documented[A] = Documented(tuple._1, tuple._2)
  }
}
