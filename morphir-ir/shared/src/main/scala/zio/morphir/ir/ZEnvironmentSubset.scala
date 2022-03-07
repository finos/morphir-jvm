/*
 * Copyright 2022 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio.morphir.ir

import zio._
import zio.prelude.AnyType

import izumi.reflect.macrortti.LightTypeTag

final class ZEnvironmentSubset[Subset[_], +R] private (
    private val map: Map[LightTypeTag, (Any, Any, Int)],
    private val index: Int,
    private var cache: Map[LightTypeTag, (Any, Any)] = Map.empty
) extends Serializable { self =>

  def ++[R1: EnvironmentTag](that: ZEnvironmentSubset[Subset, R1]): ZEnvironmentSubset[Subset, R with R1] =
    self.union[R1](that)

  /**
   * Adds a service to the environment.
   */
  def add[A](a: A)(implicit tag: Tag[A], subset: Subset[A]): ZEnvironmentSubset[Subset, R with A] =
    new ZEnvironmentSubset(self.map + (tag.tag -> ((a, subset, index))), index + 1)

  override def equals(that: Any): Boolean = that match {
    case that: ZEnvironmentSubset[_, _] => self.map == that.map
    case _                              => false
  }

  /**
   * Retrieves a service from the environment.
   */
  def get[A >: R](implicit tag: Tag[A]): (A, Subset[A]) =
    unsafeGet(tag.tag)

  override def hashCode: Int =
    map.hashCode

  /**
   * Prunes the environment to the set of services statically known to be contained within it.
   */
  def prune[R1 >: R](implicit tagged: EnvironmentTag[R1]): ZEnvironmentSubset[Subset, R1] = {
    val tag = taggedTagType(tagged)
    val set = taggedGetServices(tag)

    val missingServices = set.filterNot(tag => map.keys.exists(taggedIsSubtype(_, tag)))
    if (missingServices.nonEmpty) {
      throw new Error(
        s"Defect in zio.ZEnvironment: ${missingServices} statically known to be contained within the environment are missing"
      )
    }

    if (set.isEmpty) self
    else
      new ZEnvironmentSubset(filterKeys(self.map)(tag => set.exists(taggedIsSubtype(tag, _))), index)
        .asInstanceOf[ZEnvironmentSubset[Subset, R]]
  }

  /**
   * The size of the environment, which is the number of services contained in the environment. This is intended
   * primarily for testing purposes.
   */
  def size: Int =
    map.size

  override def toString: String =
    s"ZEnvironment($map)"

  /**
   * Combines this environment with the specified environment.
   */
  def union[R1: EnvironmentTag](that: ZEnvironmentSubset[Subset, R1]): ZEnvironmentSubset[Subset, R with R1] =
    self.unionAll[R1](that.prune)

  /**
   * Combines this environment with the specified environment. In the event of service collisions, which may not be
   * reflected in statically known types, the right hand side will be preferred.
   */
  def unionAll[R1](that: ZEnvironmentSubset[Subset, R1]): ZEnvironmentSubset[Subset, R with R1] =
    new ZEnvironmentSubset(
      self.map ++ that.map.map { case (tag, (service, subset, index)) => (tag, (service, subset, self.index + index)) },
      self.index + that.index
    )

  def unsafeGet[A](tag: LightTypeTag): (A, Subset[A]) =
    self.cache.get(tag) match {
      case Some((a, subset)) => (a.asInstanceOf[A], subset.asInstanceOf[Subset[A]])
      case None =>
        var index             = -1
        val iterator          = self.map.iterator
        var service: A        = null.asInstanceOf[A]
        var subset: Subset[A] = null.asInstanceOf[Subset[A]]
        while (iterator.hasNext) {
          val (curTag, (curService, curSubet, curIndex)) = iterator.next()
          if (taggedIsSubtype(curTag, tag) && curIndex > index) {
            index = curIndex
            service = curService.asInstanceOf[A]
            subset = curSubet.asInstanceOf[Subset[A]]
          }
        }
        if (service == null) throw new Error(s"Defect in zio.ZEnvironment: Could not find ${tag} inside ${self}")
        else {
          self.cache = self.cache + (tag -> ((service, subset)))
          (service, subset)
        }
    }

  def upcast[R1](implicit ev: R <:< R1): ZEnvironmentSubset[Subset, R1] =
    new ZEnvironmentSubset(map, index)

  /**
   * Updates a service in the environment.
   */
  def update[A >: R](f: A => A)(implicit tag: Tag[A]): ZEnvironmentSubset[Subset, R] = {
    val (a, subset) = get[A]
    add(f(a))(tag, subset)
  }

  /**
   * Filters a map by retaining only keys satisfying a predicate.
   */
  private def filterKeys[K, V](map: Map[K, V])(f: K => Boolean): Map[K, V] =
    map.foldLeft[Map[K, V]](Map.empty) { case (acc, (key, value)) =>
      if (f(key)) acc + (key -> value) else acc
    }
}

object ZEnvironmentSubset {

  /**
   * Constructs a new environment holding the single service.
   */
  def apply[Subset[_], A](a: A)(implicit tag: Tag[A], subset: Subset[A]): ZEnvironmentSubset[Subset, A] =
    new ZEnvironmentSubset[Subset, A](Map(tag.tag -> ((a, subset, 0))), 1)

  /**
   * Constructs a new environment holding the specified services. The service must be monomorphic. Parameterized
   * services are not supported.
   */
  def apply[Subset[_], A: Tag: Subset, B: Tag: Subset](a: A, b: B): ZEnvironmentSubset[Subset, A with B] =
    ZEnvironmentSubset[Subset, A](a).add[B](b)

  /**
   * Constructs a new environment holding the specified services. The service must be monomorphic. Parameterized
   * services are not supported.
   */
  def apply[Subset[_], A: Tag: Subset, B: Tag: Subset, C: Tag: Subset](
      a: A,
      b: B,
      c: C
  ): ZEnvironmentSubset[Subset, A with B with C] =
    ZEnvironmentSubset[Subset, A](a).add(b).add[C](c)

  /**
   * Constructs a new environment holding the specified services. The service must be monomorphic. Parameterized
   * services are not supported.
   */
  def apply[Subset[_], A: Tag: Subset, B: Tag: Subset, C: Tag: Subset, D: Tag: Subset](
      a: A,
      b: B,
      c: C,
      d: D
  ): ZEnvironmentSubset[Subset, A with B with C with D] =
    ZEnvironmentSubset[Subset, A](a).add(b).add(c).add[D](d)

  /**
   * Constructs a new environment holding the specified services. The service must be monomorphic. Parameterized
   * services are not supported.
   */
  def apply[
      Subset[_],
      A: Tag: Subset,
      B: Tag: Subset,
      C: Tag: Subset,
      D: Tag: Subset,
      E: Tag: Subset
  ](
      a: A,
      b: B,
      c: C,
      d: D,
      e: E
  ): ZEnvironmentSubset[Subset, A with B with C with D with E] =
    ZEnvironmentSubset[Subset, A](a).add(b).add(c).add(d).add[E](e)

  /**
   * The empty environment containing no services.
   */
  lazy val empty: ZEnvironmentSubset[AnyType, Any] =
    new ZEnvironmentSubset[AnyType, AnyRef](Map.empty, 0, Map(taggedTagType(TaggedAnyRef) -> (((), AnyType[Unit]))))

  private lazy val TaggedAnyRef: EnvironmentTag[AnyRef] =
    implicitly[EnvironmentTag[AnyRef]]
}
