package morphir.ir

import io.circe.{ Decoder, Encoder }

final case class NamedType[+A](name: Name, underlyingType: Type[A]) {
  def mapAttributes[B](f: A => B): NamedType[B] = NamedType(name, underlyingType.mapAttributes(f))
}

object NamedType {

  implicit def encodeNamedType[A: Encoder]: Encoder[NamedType[A]] =
    Encoder.encodeTuple2[Name, Type[A]].contramap(nt => (nt.name, nt.underlyingType))

  implicit def decodeNamedType[A: Decoder]: Decoder[NamedType[A]] =
    Decoder.decodeTuple2[Name, Type[A]].map { case (name, tpe) => NamedType(name, tpe) }

  def mapAttributes[A, B](f: A => B)(items: List[NamedType[A]]): List[NamedType[B]] =
    items.map(_.mapAttributes(f))
}
