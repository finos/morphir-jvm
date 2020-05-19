package morphir.ir

import enumeratum._

import scala.collection.immutable

sealed trait ExprKind {
  def isTypeExpr: Boolean
  final def isValueExpr: Boolean = !isTypeExpr
  def entryName: String
  def tag: String = entryName
}

sealed abstract class TypeExprKind extends EnumEntry with ExprKind with Product with Serializable {
  val isTypeExpr: Boolean = true
}

object TypeExprKind extends Enum[TypeExprKind] with CirceEnum[TypeExprKind] {

  val values: immutable.IndexedSeq[TypeExprKind] = findValues
  lazy val valuesSet: Set[TypeExprKind]          = values.toSet

  case object Variable         extends TypeExprKind
  case object Reference        extends TypeExprKind
  case object Tuple            extends TypeExprKind
  case object Record           extends TypeExprKind
  case object ExtensibleRecord extends TypeExprKind
  case object Function         extends TypeExprKind
  case object Unit             extends TypeExprKind

}

sealed abstract class ValueExprKind extends EnumEntry with ExprKind with Product with Serializable {
  val isTypeExpr: Boolean = false
}

object ValueExprKind extends Enum[ValueExprKind] with CirceEnum[ValueExprKind] {
  val values: immutable.IndexedSeq[ValueExprKind] = findValues
  lazy val valuesSet: Set[ValueExprKind]          = values.toSet

  case object Literal       extends ValueExprKind
  case object Constructor   extends ValueExprKind
  case object Tuple         extends ValueExprKind
  case object List          extends ValueExprKind
  case object Record        extends ValueExprKind
  case object Variable      extends ValueExprKind
  case object Reference     extends ValueExprKind
  case object Field         extends ValueExprKind
  case object FieldFunction extends ValueExprKind
  case object Apply         extends ValueExprKind
  case object Lambda        extends ValueExprKind
  case object LetDefinition extends ValueExprKind
  case object LetRecursion  extends ValueExprKind
  case object Destructure   extends ValueExprKind
  case object IfThenElse    extends ValueExprKind
  case object PatternMatch  extends ValueExprKind
  case object UpdateRecord  extends ValueExprKind
  case object Unit          extends ValueExprKind
}
