package morphir.ir

import io.circe.{Decoder, Encoder, Json}

enum TypeExprKind {
    case Variable
    case Reference
    case Tuple
    case Record
    case ExtensibleRecord
    case Function
    case Unit

    val entryName : String = toString
}

object TypeExprKind {

  implicit val encodeTypeExprKind:Encoder[TypeExprKind] = Encoder.instance(me => Json.fromString(me.entryName))

  lazy val namesToValuesMap = TypeExprKind.values.map(v => v.entryName -> v).toMap
}