package morphir.ir.syntax

import morphir.ir.ArgumentList
import scala.language.implicitConversions

trait ArgumentListSyntax {
  final implicit def morphirSyntaxArgumentList[A](args: ArgumentList[A]): ArgumentListOps[A] = new ArgumentListOps(args)
}

final class ArgumentListOps[A](val self: ArgumentList[A]) extends AnyVal {
  import ArgumentSyntax._

  def mapAttributes[B](f: A => B): ArgumentList[B] = self.map(_.mapAttributes(f))
}
