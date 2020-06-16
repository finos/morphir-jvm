package morphir.ir

import izumi.reflect.Tag
import scala.reflect.ClassTag

trait NameTag[A] {
  def name: Name
  final def tag: String               = name.toSnakeCase
  final override def toString: String = s"NameTag($tag)"
}

object NameTag {

  /** Summon an instance of a [NameTag] */
  def apply[A](implicit nameTag: NameTag[A]): NameTag[A] = nameTag

  def fromString[A](givenName: String): NameTag[A] = new NameTag[A] {
    val name: Name = Name.fromString(givenName)
  }

  def create[A](head: String, rest: String*): NameTag[A] = new NameTag[A] {
    val name: Name = Name.name(head, rest: _*)
  }

  def of[A](implicit classTag: Tag[A]): NameTag[A] = new NameTag[A] {
    val name: Name = Name.fromString(classTag.tag.shortName)
  }

  def forClass[A](implicit classTag: ClassTag[A]): NameTag[A] = new NameTag[A] {
    val name: Name = Name.fromString(classTag.runtimeClass.getSimpleName())
  }
}
