package morphir.ir

import enumeratum._

import scala.collection.immutable

sealed abstract class TypeExprKind extends EnumEntry with Product with Serializable

object TypeExprKind extends Enum[TypeExprKind] with CirceEnum[TypeExprKind] {

  val values: immutable.IndexedSeq[TypeExprKind] = findValues

  case object Variable         extends TypeExprKind
  case object Reference        extends TypeExprKind
  case object Tuple            extends TypeExprKind
  case object Record           extends TypeExprKind
  case object ExtensibleRecord extends TypeExprKind
  case object Function         extends TypeExprKind
  case object Unit             extends TypeExprKind

}
