package morphir.sdk.json

import upickle.default.{readwriter, ReadWriter => RW, macroRW}
import upickle.default._
import morphir.sdk.core.Result
import morphir.sdk.core.Result.{Err, Ok}
import scala.util.control.NonFatal
import morphir.sdk.json.Decode.Decoder.Succeed
object Decode {

  type Value = ujson.Value
  type DecodeResult[A] = Result[Error, A]
  object DecodeResult {
    def ok[A](value: => A): DecodeResult[A] =
      Result.Ok(value)

    def err[A](err: => Error): DecodeResult[A] =
      Result.Err(err)

    def errorExpecting(typeName: String, value: Value) =
      Result.Err(Error.Failure(s"Expecting $typeName", value))
  }

  sealed abstract class Decoder[A]
  object Decoder {
    private[Decode] case class Succeed[A](value: A) extends Decoder[A]
    private[Decode] case class Fail[A](message: String) extends Decoder[A]
    private[Decode] case class Null[A](value: A) extends Decoder[A]

    private[Decode] case object String extends Decoder[scala.Predef.String]
    private[Decode] case object Bool extends Decoder[Boolean]
    private[Decode] case object Int extends Decoder[Int]
    private[Decode] case object Float extends Decoder[Float]

    private[Decode] case class Value(jsonValue: Decode.Value)
        extends Decoder[Decode.Value]
  }

  def decodeString[A](decoder: Decoder[A])(input: String): DecodeResult[A] = {
    try {
      val json = read[Value](input)
      decodeValue(decoder)(json)
    } catch {
      case NonFatal(err) =>
        Error.asErrorResult(
          Error.Failure(
            "This is not valid JSON ! Error: " + err.getLocalizedMessage(),
            ujson.Str(input)
          )
        )
    }
  }

  def decodeValue[A](decoder: Decoder[A])(input: Value): DecodeResult[A] = {
    import Decoder._
    decoder match {
      case Null(value) if input.isNull => Result.Ok(value)
      case Null(value)                 => DecodeResult.errorExpecting("null", input)
      case Decoder.String =>
        input match {
          case ujson.Str(value) => DecodeResult.ok(value)
          case _                => DecodeResult.errorExpecting("a STRING", input)
        }
      case Decoder.Bool =>
        input match {
          case ujson.Bool(value) => DecodeResult.ok(value)
          case _                 => DecodeResult.errorExpecting("a BOOL", input)
        }
      case Decoder.Int =>
        input match {
          case ujson.Num(value)
              if (scala.Int.MinValue <= value && value <= scala.Int.MaxValue && value.isWhole) =>
            DecodeResult.ok(value.toInt)

          case _ => DecodeResult.errorExpecting("an INT", input)
        }
      case Decoder.Float =>
        input match {
          case ujson.Num(value)
              if (scala.Float.MinValue <= value && value <= scala.Float.MaxValue) =>
            DecodeResult.ok(value.toFloat)
          case _ => DecodeResult.errorExpecting("a FLOAT", input)
        }
      case Succeed(value)   => Result.Ok(value)
      case Fail(message)    => Result.Err(Error.Failure(message, input))
      case Value(jsonValue) => Result.Ok(jsonValue)

    }
  }

  def succeed[A](value: => A): Decoder[A] =
    Decoder.Succeed(value)

  def fail[A](message: => String): Decoder[A] =
    Decoder.Fail(message)

  /** Decode a `null` value into some actual value */
  def `null`[A](value: => A): Decoder[A] =
    Decoder.Null(value)

  /** Decode a `null` value into some actual value */
  def Null[A](value: => A): Decoder[A] =
    Decoder.Null(value)

  def value(jsonValue: Value): Decoder[Value] =
    Decoder.Value(jsonValue)

  val string: Decoder[String] = Decoder.String

  val bool: Decoder[Boolean] = Decoder.Bool

  val int: Decoder[Int] = Decoder.Int

  val float: Decoder[Float] = Decoder.Float

  //def string(input:String,)

  sealed abstract class Error {
    def asDecodeResult[A]: DecodeResult[A] =
      Err(this)
  }
  object Error {
    case class Field(name: String, error: Error) extends Error
    case class Index(index: Int, error: Error) extends Error
    case class OneOf(errors: List[Error]) extends Error
    case class Failure(message: String, json: Value) extends Error

    def asErrorResult[A](error: Error): DecodeResult[A] =
      Err(error)
  }

  def indent(str: String): String =
    str.split("\n").mkString("\n    ")

}
