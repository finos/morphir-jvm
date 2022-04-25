package morphir.sdk.list

import io.circe.{ Decoder, Encoder }
import morphir.sdk.List.List

/** Encoder and Decoder for items in a list */
object Codec {

  /**
   * Encodes a List of Items
   *
   * When passed with an encoder of a type, it returns
   * an instance of the encoder-type.
   *
   * {{{ Example Usage :: val stringEncoder = encodeList(Encoder.encodeString) }}}
   *
   *  @param encodeA where is a generic type of elements in the list
   *
   *  @return An a specific data-type encoder needed to encode list elements
   */

  implicit def encodeList[A](encodeA: Encoder[A]): Encoder[List[A]] =
    Encoder.encodeList(encodeA)

  /**
   * Decodes a json to its respective list of items
   *
   * @param decodeA
   *
   * @return A List of items of specific types
   */
  implicit def decodeList[A](decodeA: Decoder[A]): Decoder[List[A]] =
    Decoder.decodeList(decodeA)

}
