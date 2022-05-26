package org.finos.morphir.knowledge.core
import scala.annotation.tailrec
import scala.reflect.ClassTag

final case class Fields(private[knowledge] val bindings: Map[Field[_], Value] = Map.empty) { self =>

  private[knowledge] def +[A: ClassTag](kv: (Field[A], Value)): Fields = {
    val (k, v) = kv
    Fields(bindings + (k -> v))
  }

  def associateWithFields(selected: List[Field[_]])(bindWith: Field[_] => Value): Map[Field[_], Value] =
    bindings.keySet.map(field => field -> bindWith(field)).toMap

  @tailrec
  def dynamicValueOf(value: Value): Value = value match {
    case field @ Field(_, _) =>
      bindings.get(field) match {
        case Some(value) => dynamicValueOf(value)
        case None        => value
      }
    case _ => value
  }

  def fields: Set[Field[_]] = bindings.map(_._1).toSet

  def hasValue[A: ClassTag](field: Field[A]): Boolean =
    valueOf(field) match {
      case Some(value) =>
        if (value.isInstanceOf[A]) true else false
      case None => false
    }

  def keys: Set[Field[_]] = bindings.keySet

  /**
   * Look up the value of a field in the bindings. NOTE: In minikanren/microkanren, this is called `walk` or `find`.
   */
  @tailrec
  def valueOf[A: ClassTag](field: Field[A]): Option[A] =
    bindings.get(field) match {
      case Some(field @ Field(_, _)) =>
        valueOf(field.asInstanceOf[Field[A]])
      case Some(value: A) => Some(value)
      case _              => None
    }
}

object Fields {
  val empty: Fields                                               = Fields()
  private[knowledge] def init(bindings: (Field[_], Any)*): Fields = Fields(bindings.toMap)
}
