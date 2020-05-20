package morphir.ir.syntax

import morphir.ir.PatternMatchCases

trait PatternMatchCasesSyntax {
  final implicit def morphirSyntaxPatternMatchCases[A](cases: PatternMatchCases[A]): PatternMatchCasesOps[A] =
    new PatternMatchCasesOps[A](cases)
}

final class PatternMatchCasesOps[+A](val cases: PatternMatchCases[A]) extends AnyVal {
  import PatternMatchCaseSyntax._

  def mapAttributes[B](f: A => B): PatternMatchCases[B] = cases.map(_.mapAttributes(f))
}
