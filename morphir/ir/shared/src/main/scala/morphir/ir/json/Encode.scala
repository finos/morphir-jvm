package morphir.ir.json

import io.circe.syntax._
import io.circe.{ Encoder, Printer }
import morphir.ir.json.Encode.Value

trait Encode {
  def encode(value: Value, indent: Int): String = {
    val indentResolved = Math.max(0, indent)
    indentResolved match {
      case 0 => value.noSpaces
      case 2 => value.spaces2
      case 4 => value.spaces4
      case _ => value.printWith(Printer.indented(Array.fill(indent)(" ").mkString))
    }
  }

  def encode[A](value: A, indent: Int = 2)(implicit encoder: Encoder[A]): String =
    encode(value.asJson, indent)

  def encodeAsJson[A](value: A)(implicit encoder: Encoder[A]): Value =
    value.asJson
}

object Encode extends Encode {
  type Value = io.circe.Json
}
