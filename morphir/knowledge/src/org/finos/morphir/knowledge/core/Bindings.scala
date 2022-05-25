package org.finos.morphir.knowledge.core
import scala.annotation.tailrec

final case class Bindings(private[knowledge] val substitutions: Map[Field[_], Any] = Map.empty) { self =>

  def fields: Set[Field[_]] = substitutions.keySet

  def hasValue[A](field: Field[A]): Boolean =
    valueOf(field) match {
      case Some(value) =>
        if (value.isInstanceOf[A]) true else false
      case None => false
    }

  /**
   * Look up the value of a field in the bindings. NOTE: In minikanren/microkanren, this is called `walk` or `find`.
   */
  def valueOf[A](field: Field[A]): Option[A] = {
    @tailrec
    def loop[T](candidate: Field[T]): Option[T] = substitutions.get(candidate) match {
      case Some(field @ Field(_, _)) =>
        loop(field.asInstanceOf[Field[T]])
      case Some(value) =>
        if (value.getClass() == field.fieldType.runtimeClass) Some[T](value.asInstanceOf[T]) else None
      case None => None
    }
    loop(field)
  }
}

object Bindings {
  val empty: Bindings                                                      = Bindings()
  private[knowledge] def having(substitutions: (Field[_], Any)*): Bindings = Bindings(substitutions.toMap)
}
