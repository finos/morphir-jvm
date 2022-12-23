package morphir.sdk.tuple

import io.circe.{ Decoder, Encoder, HCursor, Json }
import morphir.sdk.Tuple.Tuple

object Codec {

  /**
   * Encodes a Tuple of 2 items
   *
   * @return
   *   Returns an array of each item encoded by its type
   */
  implicit def encodeTuple[A0, A1](
    encodeA0: Encoder[A0],
    encodeA1: Encoder[A1]
  ): Encoder[Tuple[A0, A1]] =
    (a: Tuple[A0, A1]) =>
      Json.arr(
        encodeA0(a._1),
        encodeA1(a._2)
      )

  implicit def encodeTuple[A0, A1, A2](
    encodeA0: Encoder[A0],
    encodeA1: Encoder[A1],
    encodeA2: Encoder[A2]
  ): Encoder[Tuple3[A0, A1, A2]] =
    (a: Tuple3[A0, A1, A2]) =>
      Json.arr(
        encodeA0(a._1),
        encodeA1(a._2),
        encodeA2(a._3)
      )

  /**
   * Decode an array of json to a Tuple
   *
   * @return
   *   Returns values of json array as Tuples
   */
  implicit def decodeTuple[A0, A1](
    decodeA0: Decoder[A0],
    decodeA1: Decoder[A1]
  ): Decoder[Tuple[A0, A1]] =
    (cursor: HCursor) =>
      for {
        arg1 <- cursor.downN(0).as(decodeA0)
        arg2 <- cursor.downN(1).as(decodeA1)
      } yield morphir.sdk.Tuple.pair(arg1)(arg2)

  implicit def decodeTuple[A0, A1, A2](
    decodeA0: Decoder[A0],
    decodeA1: Decoder[A1],
    decodeA2: Decoder[A2]
  ): Decoder[Tuple3[A0, A1, A2]] =
    (cursor: HCursor) =>
      for {
        arg1 <- cursor.downN(0).as(decodeA0)
        arg2 <- cursor.downN(1).as(decodeA1)
        arg3 <- cursor.downN(2).as(decodeA2)
      } yield ((arg1, arg2, arg3))
}
