package morphir.ir.literal

/** Generated based on IR.Literal
*/
object Codec{

  implicit val encodeLiteral: io.circe.Encoder[morphir.ir.Literal.Literal] = ((literal: morphir.ir.Literal.Literal) =>
    literal match {
      case morphir.ir.Literal.BoolLiteral(arg1) => 
        io.circe.Json.arr(
          io.circe.Json.fromString("BoolLiteral"),
          morphir.sdk.basics.Codec.encodeBool(arg1)
        )
      case morphir.ir.Literal.CharLiteral(arg1) => 
        io.circe.Json.arr(
          io.circe.Json.fromString("CharLiteral"),
          morphir.sdk.char.Codec.encodeChar(arg1)
        )
      case morphir.ir.Literal.DecimalLiteral(arg1) => 
        io.circe.Json.arr(
          io.circe.Json.fromString("DecimalLiteral"),
          morphir.sdk.decimal.Codec.encodeDecimal(arg1)
        )
      case morphir.ir.Literal.FloatLiteral(arg1) => 
        io.circe.Json.arr(
          io.circe.Json.fromString("FloatLiteral"),
          morphir.sdk.basics.Codec.encodeFloat(arg1)
        )
      case morphir.ir.Literal.StringLiteral(arg1) => 
        io.circe.Json.arr(
          io.circe.Json.fromString("StringLiteral"),
          morphir.sdk.string.Codec.encodeString(arg1)
        )
      case morphir.ir.Literal.WholeNumberLiteral(arg1) => 
        io.circe.Json.arr(
          io.circe.Json.fromString("WholeNumberLiteral"),
          morphir.sdk.basics.Codec.encodeInt(arg1)
        )
    })
  
  implicit val decodeLiteral: io.circe.Decoder[morphir.ir.Literal.Literal] = ((c: io.circe.HCursor) =>
    c.withFocus(_.withString(((str) =>
      io.circe.Json.arr(io.circe.Json.fromString(str))))).downN(0).as(morphir.sdk.string.Codec.decodeString).flatMap(((tag) =>
      tag match {
        case "BoolLiteral" => 
          for {
            arg1 <- c.downN(1).as(morphir.sdk.basics.Codec.decodeBool)
          }  yield morphir.ir.Literal.BoolLiteral(arg1)
        case "CharLiteral" => 
          for {
            arg1 <- c.downN(1).as(morphir.sdk.char.Codec.decodeChar)
          }  yield morphir.ir.Literal.CharLiteral(arg1)
        case "DecimalLiteral" => 
          for {
            arg1 <- c.downN(1).as(morphir.sdk.decimal.Codec.decodeDecimal)
          }  yield morphir.ir.Literal.DecimalLiteral(arg1)
        case "FloatLiteral" => 
          for {
            arg1 <- c.downN(1).as(morphir.sdk.basics.Codec.decodeFloat)
          }  yield morphir.ir.Literal.FloatLiteral(arg1)
        case "StringLiteral" => 
          for {
            arg1 <- c.downN(1).as(morphir.sdk.string.Codec.decodeString)
          }  yield morphir.ir.Literal.StringLiteral(arg1)
        case "WholeNumberLiteral" => 
          for {
            arg1 <- c.downN(1).as(morphir.sdk.basics.Codec.decodeInt)
          }  yield morphir.ir.Literal.WholeNumberLiteral(arg1)
      })))

}