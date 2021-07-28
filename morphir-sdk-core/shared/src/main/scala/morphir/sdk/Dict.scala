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

package morphir.sdk

import morphir.sdk.Maybe._

object Dict {
  sealed abstract class Dict[K, +V]
  private case object EmptyDict                              extends Dict[Any, Nothing]
  private case class DictImpl[K, +V](val wrapped: Map[K, V]) extends Dict[K, V]

  def empty[K, V]: Dict[K, V] = EmptyDict.asInstanceOf[Dict[K, V]]

  def get[K, V](targetKey: K)(dict: Dict[K, V]): Maybe[V] =
    dict match {
      case EmptyDict         => Maybe.Nothing
      case DictImpl(wrapped) => wrapped.get(targetKey)
    }

  def member[K, V](key: K)(dict: Dict[K, V]): Boolean =
    dict match {
      case EmptyDict         => false
      case DictImpl(wrapped) => wrapped.contains(key)
    }

  def size[K, V](dict: Dict[K, V]): Int = dict match {
    case EmptyDict         => 0
    case DictImpl(wrapped) => wrapped.size
  }

  def isEmpty[K, V](dict: Dict[K, V]): Boolean =
    dict match {
      case EmptyDict         => true
      case DictImpl(wrapped) => wrapped.isEmpty
    }

  def insert[K, V](key: K)(value: V)(dict: Dict[K, V]): Dict[K, V] =
    dict match {
      case EmptyDict         => DictImpl(Map(key -> value))
      case DictImpl(wrapped) => DictImpl(wrapped + (key -> value))
    }

  def fromList[K, V](assocs: List[(K, V)]): Dict[K, V] =
    assocs match {
      case Nil => empty[K, V]
      case xs  => DictImpl(xs.toMap)
    }

  object tupled {

    @inline def get[K, V](targetKey: K, dict: Dict[K, V]): Maybe[V] =
      Dict.get(targetKey)(dict)

    @inline def member[K, V](key: K, dict: Dict[K, V]): Boolean =
      Dict.member(key)(dict)

    @inline def insert[K, V](key: K, value: V, dict: Dict[K, V]): Dict[K, V] =
      Dict.insert(key)(value)(dict)
  }
}
