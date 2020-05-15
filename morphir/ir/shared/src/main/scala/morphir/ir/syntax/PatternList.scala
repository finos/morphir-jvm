package morphir.ir.syntax

import morphir.ir.PatternList
import scala.language.implicitConversions
trait PatternListSyntax {
  final implicit def morphirSyntaxPatternList[A](list: PatternList[A]): PatternListOps[A] =
    new PatternListOps[A](list)
}

final class PatternListOps[+A](val self: PatternList[A]) extends AnyVal {
  def mapAttributes[B](f: A => B): PatternList[B] = self.map(_.mapAttributes(f))
}
