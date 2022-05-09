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
  sealed abstract class Dict[-K, +V]
  private case object EmptyDict                          extends Dict[Any, Nothing]
  private case class DictImpl[K, +V](wrapped: Map[K, V]) extends Dict[K, V]

  /* Build */
  def empty[K, V]: Dict[K, V] = EmptyDict

  def singleton[K, V](key: K)(value: V): Dict[K, V] = DictImpl(Map(key -> value))

  def insert[K, V](key: K)(value: V)(dict: Dict[K, V]): Dict[K, V] =
    dict match {
      case EmptyDict         => DictImpl(Map(key -> value))
      case DictImpl(wrapped) => DictImpl(wrapped + (key -> value))
    }

  def update[K, V](targetKey: K)(updatedValue: V)(dict: Dict[K, V]): Dict[K, V] = dict match {
    case EmptyDict         => dict
    case DictImpl(wrapped) => DictImpl(wrapped.updated(targetKey, updatedValue))
  }

  def remove[K, V](targetKey: K)(dict: Dict[K, V]): Dict[K, V] = dict match {
    case EmptyDict => dict
    case DictImpl(wrapped) =>
      val res = wrapped.filterKeys(key => key != targetKey)
      res match {
        case Map.empty => EmptyDict
        case resultMap => DictImpl(resultMap)
      }
  }

  /* Query*/
  def isEmpty[K, V](dict: Dict[K, V]): Boolean =
    dict match {
      case EmptyDict         => true
      case DictImpl(wrapped) => wrapped.isEmpty
    }

  def member[K, V](key: K)(dict: Dict[K, V]): Boolean =
    dict match {
      case EmptyDict         => false
      case DictImpl(wrapped) => wrapped.contains(key)
    }

  def get[K, V](targetKey: K)(dict: Dict[K, V]): Maybe[V] =
    dict match {
      case EmptyDict         => Maybe.Nothing
      case DictImpl(wrapped) => wrapped.get(targetKey)
    }

  def size[K, V](dict: Dict[K, V]): Int = dict match {
    case EmptyDict         => 0
    case DictImpl(wrapped) => wrapped.size
  }

  /* List */
  def keys[K, V](dict: Dict[K, V]): List[K] = dict match {
    case EmptyDict         => List.empty[K]
    case DictImpl(wrapped) => wrapped.keys.toList
  }

  def values[K, V](dict: Dict[K, V]): List[V] = dict match {
    case EmptyDict         => List.empty[V]
    case DictImpl(wrapped) => wrapped.values.toList
  }

  def toList[K, V](dict: Dict[K, V]): List[(K, V)] =
    dict match {
      case EmptyDict =>
        List()
      case DictImpl(map) =>
        map.toList
    }

  def fromList[K, V](assocs: List[(K, V)]): Dict[K, V] =
    assocs match {
      case Nil => empty[K, V]
      case xs  => DictImpl(xs.toMap)
    }

  /* Transform */
  def map[K, V, B](f: K => V => B)(dict: Dict[K, V]): Dict[K, B] = dict match {
    case EmptyDict         => empty
    case DictImpl(wrapped) => DictImpl(wrapped.map(x => (x._1, f(x._1)(x._2))))
  }

  def foldl[K, V, B](f: K => V => B => B)(initValue: B)(dict: Dict[K, V]): B = dict match {
    case EmptyDict => initValue
    case DictImpl(wrapped) =>
      wrapped.foldLeft(initValue)((accumulator, pairedValues) => f(pairedValues._1)(pairedValues._2)(accumulator))
  }

  def foldr[K, V, B](f: K => V => B => B)(initValue: B)(dict: Dict[K, V]): B = dict match {
    case EmptyDict => initValue
    case DictImpl(wrapped) =>
      wrapped.foldRight(initValue)((a, accumulator) => f(a._1)(a._2)(accumulator))
  }

  def filter[K, V](f: K => V => Boolean)(dict: Dict[K, V]): Dict[K, V] = dict match {
    case EmptyDict => empty
    case DictImpl(wrapped) =>
      val filtered = wrapped.filter(x => f(x._1)(x._2))
      if (filtered.isEmpty) empty else DictImpl(filtered)
  }

  def partition[K, V](f: K => V => Boolean)(dict: Dict[K, V]): (Dict[K, V], Dict[K, V]) =
    dict match {
      case EmptyDict => (EmptyDict, dict)
      case DictImpl(wrapped) =>
        val result = wrapped.partition(x => f(x._1)(x._2))
        (DictImpl(result._1), DictImpl(result._2))
    }

  /* Combine */
  def union[K, V](dictToMerged: Dict[K, V])(dict: Dict[K, V]): Dict[K, V] = dict match {
    case EmptyDict         => EmptyDict
    case DictImpl(wrapped) => wrapped ++ dictToMerged
  }

  def intersect[K, V](dictToIntersect: Dict[K, V])(dict: Dict[K, V]): Dict[K, V] = dict match {
    case EmptyDict => EmptyDict
    case DictImpl(wrapped) =>
      DictImpl(wrapped.toSet.intersect(Dict.toList(dictToIntersect).toSet).toMap)
  }

  def diff[K, V](dictToDiff: Dict[K, V])(dict: Dict[K, V]): Dict[K, V] = dict match {
    case EmptyDict         => EmptyDict
    case DictImpl(wrapped) => DictImpl(wrapped.toSet.diff(Dict.toList(dictToDiff).toSet).toMap)
  }

  object tupled {

    @inline def get[K, V](targetKey: K, dict: Dict[K, V]): Maybe[V] =
      Dict.get(targetKey)(dict)

    @inline def member[K, V](key: K, dict: Dict[K, V]): Boolean =
      Dict.member(key)(dict)

    @inline def insert[K, V](
      key: K,
      value: V,
      dict: Dict[K, V]
    ): Dict[K, V] =
      Dict.insert(key)(value)(dict)
  }
}
