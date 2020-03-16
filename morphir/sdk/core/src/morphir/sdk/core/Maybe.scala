package morphir.sdk.core

object Maybe {

  val Nothing: Nothing = None

  def nothing[A]: Maybe[A] = Nothing.asInstanceOf[Maybe[A]]

  def just[A](value: A): Maybe[A] = Some(value)

  def map[A, A1](fn: A => A1): Maybe[A] => Maybe[A1] =
    (value: Maybe[A]) =>
      value match {
        case Just(a) => Just(fn(a))
        case _       => Nothing.asInstanceOf[Maybe[A1]]
      }

  def map2[A, B, V](fn: A => B => V): Maybe[A] => Maybe[B] => Maybe[V] =
    (maybeA: Maybe[A]) =>
      (maybeB: Maybe[B]) =>
        (maybeA, maybeB) match {
          case (Just(a), Just(b)) => Just(fn(a)(b))
          case _                  => Nothing.asInstanceOf[Maybe[V]]
        }

  def map3[A, B, C, V](
      fn: A => B => C => V
  ): Maybe[A] => Maybe[B] => Maybe[C] => Maybe[V] =
    (maybeA: Maybe[A]) =>
      (maybeB: Maybe[B]) =>
        (maybeC: Maybe[C]) =>
          (maybeA, maybeB, maybeC) match {
            case (Just(a), Just(b), Just(c)) => Just(fn(a)(b)(c))
            case _                           => Nothing.asInstanceOf[Maybe[V]]
          }

  def map4[A, B, C, D, V](
      fn: A => B => C => D => V
  ): Maybe[A] => Maybe[B] => Maybe[C] => Maybe[D] => Maybe[V] =
    (maybeA: Maybe[A]) =>
      (maybeB: Maybe[B]) =>
        (maybeC: Maybe[C]) =>
          (maybeD: Maybe[D]) =>
            (maybeA, maybeB, maybeC, maybeD) match {
              case (Just(a), Just(b), Just(c), Just(d)) => Just(fn(a)(b)(c)(d))
              case _                                    => Nothing.asInstanceOf[Maybe[V]]
            }

  def map5[A, B, C, D, E, V](
      fn: A => B => C => D => E => V
  ): Maybe[A] => Maybe[B] => Maybe[C] => Maybe[D] => Maybe[E] => Maybe[V] =
    (maybeA: Maybe[A]) =>
      (maybeB: Maybe[B]) =>
        (maybeC: Maybe[C]) =>
          (maybeD: Maybe[D]) =>
            (maybeE: Maybe[E]) =>
              (maybeA, maybeB, maybeC, maybeD, maybeE) match {
                case (Just(a), Just(b), Just(c), Just(d), Just(e)) =>
                  Just(fn(a)(b)(c)(d)(e))
                case nothing => Nothing.asInstanceOf[Maybe[V]]
              }

  def andThen[A, B](fn: A => Maybe[B]): Maybe[A] => Maybe[B] =
    (maybeA: Maybe[A]) =>
      maybeA match {
        case Just(value) => fn(value)
        case Nothing     => Nothing.asInstanceOf[Maybe[B]]
      }

  def withDefault[A, A1 >: A](defaultValue: A1) =
    (maybeValue: Maybe[A]) =>
      maybeValue match {
        case _: Maybe.Nothing => defaultValue
        case Just(value)      => value
      }

  type Just[+A] = Some[A]

  object Just {
    def apply[A](value: A): Just[A] = Some(value)
    def unapply[A](value: Maybe[A]): Option[A] =
      value
  }

  type Nothing = None.type

}
