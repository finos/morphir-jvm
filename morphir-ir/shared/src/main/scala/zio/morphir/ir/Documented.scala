package zio.morphir.ir
import zio.prelude.*

final case class Documented[+A](doc: String, value: A) {
  def map[B](f: A => B): Documented[B]                 = Documented(doc, f(value))
  def flatMap[B](f: A => Documented[B]): Documented[B] = f(value)
}

object Documented {
  implicit val DocumentedCovariant: Covariant[Documented] = new Covariant[Documented] {
    def map[A, B](f: A => B): Documented[A] => Documented[B] = _.map(f)
  }

  implicit val DocumentedForEach: ForEach[Documented] =
    new ForEach[Documented] {
      def forEach[G[+_]: IdentityBoth: Covariant, A, B](self: Documented[A])(f: A => G[B]): G[Documented[B]] =
        f(self.value).map(Documented(self.doc, _))
    }
}
