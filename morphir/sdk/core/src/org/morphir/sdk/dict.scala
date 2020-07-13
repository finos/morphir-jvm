package org.morphir.sdk

import maybe._

object dict {
  sealed abstract class Dict[K, +V]
  private case object EmptyDict                              extends Dict[Any, Nothing]
  private case class DictImpl[K, +V](val wrapped: Map[K, V]) extends Dict[K, V]

  def empty[K, V]: Dict[K, V] = EmptyDict.asInstanceOf[Dict[K, V]]

  def get[K, V](targetKey: K)(dict: Dict[K, V]): Maybe[V] =
    dict match {
      case EmptyDict         => maybe.Nothing
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
      dict.get(targetKey)(dict)

    @inline def member[K, V](key: K, dict: Dict[K, V]): Boolean =
      dict.member(key)(dict)

    @inline def insert[K, V](key: K, value: V, dict: Dict[K, V]): Dict[K, V] =
      dict.insert(key)(value)(dict)
  }
}
