/*
Copyright 2020 Morgan Stanley

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/


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
