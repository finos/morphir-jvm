package org.finos.morphir.knowledge.core
import scala.annotation.tailrec
import scala.reflect.ClassTag

final case class Bindings(private[knowledge] val substitutions: Map[Field[_], Any] = Map.empty) { self =>

  private[knowledge] def +[A: ClassTag](kv: (Field[A], Any)): Bindings = {
    val (k, v) = kv
    Bindings(substitutions + (k -> v))
  }

  @tailrec
  def dynamicValueOf(value: Value): Value = value match {
    case field @ Field(_, _) =>
      substitutions.get(field) match {
        case Some(value) => dynamicValueOf(value)
        case None        => value
      }
    case _ => value
  }

  def fields: Set[Field[_]] = substitutions.keySet

  def hasValue[A: ClassTag](field: Field[A]): Boolean =
    valueOf(field) match {
      case Some(value) =>
        if (value.isInstanceOf[A]) true else false
      case None => false
    }

  /**
   * Look up the value of a field in the bindings. NOTE: In minikanren/microkanren, this is called `walk` or `find`.
   */
  @tailrec
  def valueOf[A: ClassTag](field: Field[A]): Option[A] =
    substitutions.get(field) match {
      case Some(field @ Field(_, _)) =>
        valueOf(field.asInstanceOf[Field[A]])
      case Some(value: A) => Some(value)
      case _              => None
    }
}

object Bindings {
  val empty: Bindings                                                      = Bindings()
  private[knowledge] def having(substitutions: (Field[_], Any)*): Bindings = Bindings(substitutions.toMap)
}
