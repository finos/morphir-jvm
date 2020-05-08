package morphir.ir

import io.circe.{Decoder, Encoder, Json}

sealed trait ExprKind:
  def isTypeExpr: Boolean
  final def isValueExpr: Boolean = !isTypeExpr

enum TypeExprKind           extends  ExprKind:
    case Variable           extends TypeExprKind
    case Reference          extends TypeExprKind
    case Tuple              extends TypeExprKind
    case Record             extends TypeExprKind
    case ExtensibleRecord   extends TypeExprKind
    case Function           extends TypeExprKind
    case Unit               extends TypeExprKind

    def entryName : String = toString
    def isTypeExpr: Boolean = true

object TypeExprKind:
  implicit val encodeTypeExprKind:Encoder[TypeExprKind] = Encoder.instance(me => Json.fromString(me.entryName))
  implicit val decodeTypeExprKind:Decoder[TypeExprKind] = Decoder.decodeString.emapTry(name => scala.util.Try(valueOf(name)))

  lazy val namesToValuesMap = TypeExprKind.values.map(v => v.entryName -> v).toMap
  lazy val valuesSet = values.toSet

  inline def withName(name:String):TypeExprKind = valueOf(name)
  def withNameOption(name:String):Option[TypeExprKind] = namesToValuesMap.get(name)

enum ValueExprKind extends ExprKind:
  case Literal extends ValueExprKind
  case Constructor extends ValueExprKind
  case Tuple extends ValueExprKind
  case List extends ValueExprKind
  case Record extends ValueExprKind
  case Variable extends ValueExprKind
  case Reference extends ValueExprKind
  case Field extends ValueExprKind
  case FieldFunction extends ValueExprKind
  case Apply extends ValueExprKind
  case Lambda extends ValueExprKind
  case LetDefinition extends ValueExprKind
  case LetRecursion extends ValueExprKind
  case Destructure extends ValueExprKind
  case IfThenElse extends ValueExprKind
  case PatternMatch extends ValueExprKind
  case UpdateRecord extends ValueExprKind
  case Unit extends ValueExprKind

  val entryName: String = toString
  val isTypeExpr: Boolean = false

object ValueExprKind:

  implicit val encodeValueExprKind:Encoder[ValueExprKind] = Encoder.instance(me => Json.fromString(me.entryName))

  lazy val namesToValuesMap = ValueExprKind.values.map(v => v.entryName -> v).toMap
  lazy val valuesSet = values.toSet

  inline def withName(name:String):ValueExprKind = valueOf(name)
  def withNameOption(name:String):Option[ValueExprKind] = namesToValuesMap.get(name)