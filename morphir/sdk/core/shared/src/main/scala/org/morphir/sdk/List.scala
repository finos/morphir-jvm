package org.morphir.sdk

object List {
  type List[+A] = scala.List[A]

  def apply[A](items: A*): List[A] =
    scala.List(items: _*)

  @inline def empty[A]: List[A] = Nil

  @inline def singleton[A](item: A): List[A] = scala.List(item)

  @inline def repeat[A](n: Int)(elem: => A): List[A] =
    scala.List.fill(n)(elem)

  def range(start: Int)(end: Int): List[Int] =
    scala.List.range(start, end)

  @inline def cons[A](head: A)(tail: List[A]): List[Any] = head :: tail

  def indexedMap[X, R](fn: Int => X => R)(xs: List[X]): List[R] =
    xs.zipWithIndex.map(tuple => fn(tuple._2)(tuple._1))

  @inline def map[A, B](mapping: A => B)(list: List[A]): List[B] =
    list.map(mapping)

  /**
    * Combine two lists, combining them with the given function. If one list is longer, the extra elements are dropped.
    * @param mapping a mapping function
    * @param xs the first list
    * @param ys the second list
    * @tparam A the type of the first list
    * @tparam B the type of the second list
    * @tparam R the type of the resulting list
    * @return a list containing the combined elements of list1 and list2 using the mapping function.
    */
  def map2[A, B, R](
      mapping: A => B => R
  )(xs: List[A])(ys: List[B]): List[R] =
    xs.zip(ys).map {
      case (a, b) => mapping(a)(b)
    }

  def map3[X, Y, Z, R](
      mapping: X => Y => Z => R
  )(xs: List[X])(ys: List[Y])(zs: List[Z]): List[R] =
    xs.zip(ys).zip(zs).map {
      case ((x, y), z) => mapping(x)(y)(z)
    }

  def map4[A, B, C, D, R](
      mapping: A => B => C => D => R
  )(as: List[A])(bs: List[B])(cs: List[C])(ds: List[D]): List[R] =
    as.zip(bs).zip(cs).zip(ds).map {
      case (((a, b), c), d) => mapping(a)(b)(c)(d)
    }

  def map5[A, B, C, D, E, R](
      mapping: A => B => C => D => E => R
  )(as: List[A])(bs: List[B])(cs: List[C])(ds: List[D])(es: List[E]): List[R] =
    as.zip(bs)
      .zip(cs)
      .zip(ds)
      .zip(es)
      .map {
        case ((((a, b), c), d), e) => mapping(a)(b)(c)(d)(e)
      }
      .toList
}
