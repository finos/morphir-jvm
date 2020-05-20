package morphir.ir.syntax

import morphir.ir.ValueExprList

trait ValueExprListSyntax {
  implicit final def morphirSyntaxValueExprList[A](list: ValueExprList[A]): ValueExprListOps[A] =
    new ValueExprListOps[A](list)
}

final class ValueExprListOps[+A](val self: ValueExprList[A]) extends AnyVal {
  def mapAttributes[B](f: A => B): ValueExprList[B] =
    self.map(_.mapAttributes(f))
}
