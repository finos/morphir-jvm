package morphir.ir.literal

/** Generated based on IR.Literal
 */
object Codec{

  implicit val encodeLiteral: io.circe.Encoder[morphir.ir.Literal.Literal] = ((literal: morphir.ir.Literal.Literal) =>
    literal match {
      case morphir.ir.Literal.BoolLiteral(value) =>
        io.circe.Json.arr(
          io.circe.Json.fromString("""BoolLiteral"""),
          morphir.sdk.basics.Codec.encodeBool(value)
        )
      case morphir.ir.Literal.CharLiteral(value) =>
        io.circe.Json.arr(
          io.circe.Json.fromString("""CharLiteral"""),
          morphir.sdk.char.Codec.encodeChar(value)
        )
      case morphir.ir.Literal.DecimalLiteral(value) =>
        io.circe.Json.arr(
          io.circe.Json.fromString("""DecimalLiteral"""),
          morphir.sdk.decimal.Codec.encodeDecimal(value)
        )
      case morphir.ir.Literal.FloatLiteral(value) =>
        io.circe.Json.arr(
          io.circe.Json.fromString("""FloatLiteral"""),
          morphir.sdk.basics.Codec.encodeFloat(value)
        )
      case morphir.ir.Literal.StringLiteral(value) =>
        io.circe.Json.arr(
          io.circe.Json.fromString("""StringLiteral"""),
          morphir.sdk.string.Codec.encodeString(value)
        )
      case morphir.ir.Literal.WholeNumberLiteral(value) =>
        io.circe.Json.arr(
          io.circe.Json.fromString("""WholeNumberLiteral"""),
          morphir.sdk.basics.Codec.encodeInt(value)
        )
    })

  implicit val decodeLiteral: io.circe.Decoder[morphir.ir.Literal.Literal] = ((c: io.circe.HCursor) =>
    c.withFocus(_.withString(((str) =>
      io.circe.Json.arr(io.circe.Json.fromString(str))))).downN(0).as(morphir.sdk.string.Codec.decodeString).flatMap(((tag) =>
      tag match {
        case """BoolLiteral""" =>
          for {
            value <- c.downN(1).as(morphir.sdk.basics.Codec.decodeBool)
          }  yield morphir.ir.Literal.BoolLiteral(value)
        case """CharLiteral""" =>
          for {
            value <- c.downN(1).as(morphir.sdk.char.Codec.decodeChar)
          }  yield morphir.ir.Literal.CharLiteral(value)
        case """DecimalLiteral""" =>
          for {
            value <- c.downN(1).as(morphir.sdk.decimal.Codec.decodeDecimal)
          }  yield morphir.ir.Literal.DecimalLiteral(value)
        case """FloatLiteral""" =>
          for {
            value <- c.downN(1).as(morphir.sdk.basics.Codec.decodeFloat)
          }  yield morphir.ir.Literal.FloatLiteral(value)
        case """StringLiteral""" =>
          for {
            value <- c.downN(1).as(morphir.sdk.string.Codec.decodeString)
          }  yield morphir.ir.Literal.StringLiteral(value)
        case """WholeNumberLiteral""" =>
          for {
            value <- c.downN(1).as(morphir.sdk.basics.Codec.decodeInt)
          }  yield morphir.ir.Literal.WholeNumberLiteral(value)
      })))

}