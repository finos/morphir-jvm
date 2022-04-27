package morphir.sdk.result

import io.circe.{Decoder, HCursor, Encoder, Json}
import morphir.sdk.Result
import morphir.sdk.Result.{Err, Ok}


object Codec {
  implicit def encodeResult[E, A](implicit encodeE: Encoder[E], encodeA: Encoder[A]): Encoder[Result[E, A]] = (result:  Result[E, A]) =>
    result match {
      case Ok(value) =>
        Json.arr(
          Json.fromString("Ok"),
          encodeA(value)
        )
      case Err(error) =>
        Json.arr(
          Json.fromString("Err"),
          encodeE(error)
        )
    }

  implicit def decodeResult[E, A](implicit decodeE: Decoder[E], decodeA: Decoder[A]): Decoder[Result[E, A]] = (c: HCursor) =>
    c.downN(0).as(decodeE).flatMap { tag =>
      tag match {
        case "Ok" =>
          for {
            a <- c.downN(1).as(decodeA)
          } yield Ok(a)
        case "Err" =>
          for {
            e <- c.downN(1).as(decodeE)
          }  yield  Err(e)
      }
    }
}


