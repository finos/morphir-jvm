package foo

import io.circe.{ Decoder, Encoder, HCursor, Json }
import morphir.sdk.string.Codec.{ decodeString, encodeString }
import morphir.sdk.basics.Codec.{ decodeInt, encodeInt }

object Codec {

  implicit val decodeFoo: Decoder[Foo] = (c: HCursor) =>
    for {
      field1 <- c.downField("field1").as(decodeString)
      field2 <- c.downField("field2").as(decodeString)
      field3 <- c.downField("field3").as(decodeInt)
    } yield Foo(field1, field2, field3)

  implicit val encodeFoo: Encoder[Foo] = (a: Foo) =>
    Json.obj(
      ("field1", encodeString(a.field1)),
      ("field2", encodeString(a.field2)),
      ("field3", encodeInt(a.field3))
    )

  implicit val decodeBar: Decoder[Bar] = (c: HCursor) =>
    c.downN(0).as[String].flatMap { tag =>
      tag match {
        case "Baz" =>
          for {
            arg1 <- c.downN(1).as(decodeString)
          } yield Baz(arg1)
        case "Bat" =>
          for {
            arg1 <- c.downN(1).as(decodeString)
            arg2 <- c.downN(2).as(decodeInt)
          } yield Bat(arg1, arg2)
      }
    }

  implicit val encodeBar: Encoder[Bar] = (a: Bar) =>
    a match {
      case Baz(arg1) =>
        Json.arr(
          Json.fromString("Baz"),
          encodeString(arg1)
        )
      case Bat(arg1, arg2) =>
        Json.arr(
          Json.fromString("Bat"),
          encodeString(arg1),
          encodeInt(arg2)
        )
    }

}
