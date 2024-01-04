package object millbuild {
  implicit final class SeqOps[A](val self: Seq[A]) extends AnyVal {
    def concatIf(condition: => Boolean)(values: A*): Seq[A] =
      if (condition) self ++ values else self
  }
}
