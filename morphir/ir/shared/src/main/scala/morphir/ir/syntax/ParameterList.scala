package morphir.ir.syntax
import morphir.ir.ParameterList

import scala.language.implicitConversions
trait ParameterListSyntax {
  final implicit def morphirSyntaxParameterList[A](parameters: ParameterList[A]): ParameterListOps[A] =
    new ParameterListOps[A](parameters)
}

final class ParameterListOps[A](val self: ParameterList[A]) extends AnyVal {
  import ParameterSyntax._
  def mapAttributes[B](f: A => B): ParameterList[B] = self.map(_.mapAttributes(f))
}
