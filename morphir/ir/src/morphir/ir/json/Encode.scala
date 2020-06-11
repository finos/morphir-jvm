package morphir.ir.json

import upickle.default._
import morphir.ir.json.Encode.Value

trait Encode {
  type Encoder[A] = Writer[A]

  def compactEncode(value: Value): String                              = write(value)
  def compactEncode[A](value: A)(implicit encoder: Encoder[A]): String = write(value)

  def encode(value: Value, indent: Int): String = write(value, indent)

  def encode[A](value: A, indent: Int = 2)(implicit encoder: Encoder[A]): String =
    write(value, indent)

  def encodeAsJson[A](value: A)(implicit encoder: Encoder[A]): Value =
    writeJs(value)
}

object Encode extends Encode {
  type Value = ujson.Value
}
