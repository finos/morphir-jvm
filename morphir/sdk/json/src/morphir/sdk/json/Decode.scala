package morphir.sdk.json

import upickle.default.{readwriter, ReadWriter => RW, macroRW}
import upickle.default._

object Decode {
  type Value = ujson.Value

  sealed abstract class Error
  object Error {
    case class Field(name: String, error: Error) extends Error
    case class Index(index: Int, error: Error) extends Error
    case class OneOf(errors: List[Error]) extends Error
    case class Failure(message: String, json: Value) extends Error
  }

  def indent(str: String): String =
    str.split("\n").mkString("\n    ")
}
