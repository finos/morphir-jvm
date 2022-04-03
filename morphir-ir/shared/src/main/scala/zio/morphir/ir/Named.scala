package zio.morphir.ir

final case class Named[+A](name: Name, value: A) { self =>
  def map[B](f: A => B): Named[B]            = Named(name, f(value))
  def flatMap[B](f: A => Named[B]): Named[B] = f(value)
}

object Named {
  def apply[A](name: String, value: A): Named[A] = Named(Name.fromString(name), value)
}
