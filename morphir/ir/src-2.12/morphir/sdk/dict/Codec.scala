package morphir.sdk.dict

import io.circe.{ Decoder, Encoder }
import morphir.sdk.Dict
import morphir.sdk.Dict.Dict

object Codec {

  implicit def decodeDict[K, V](implicit decodeKey: Decoder[K], decodeValue: Decoder[V]): Decoder[Dict[K, V]] =
    Decoder.decodeList(Decoder.decodeTuple2(decodeKey, decodeValue)).map(Dict.fromList)

  implicit def encodeDict[K, V](implicit encodeKey: Encoder[K], encodeValue: Encoder[V]): Encoder[Dict[K, V]] =
    Encoder.encodeList(Encoder.encodeTuple2(encodeKey, encodeValue)).contramap(Dict.toList)

}
