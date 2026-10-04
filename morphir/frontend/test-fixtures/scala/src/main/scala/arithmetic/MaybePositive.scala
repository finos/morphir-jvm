package arithmetic

object MaybePositive:
  def maybePositive(a: Int): Option[Int] =
    if a > 0 then Some(a)
    else None
