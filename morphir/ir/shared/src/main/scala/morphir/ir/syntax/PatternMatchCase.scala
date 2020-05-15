package morphir.ir.syntax

import morphir.ir.{ Pattern, PatternMatchCase, Value }
import scala.language.implicitConversions

trait PatternMatchCaseSyntax {
  final implicit def morphirSyntaxPatternMatchCase[A](
    patternMatchCase: PatternMatchCase[A]
  ): PatternMatchCaseOps[A] = new PatternMatchCaseOps[A](patternMatchCase)
}

object PatternMatchCaseSyntax extends PatternMatchCaseSyntax

final class PatternMatchCaseOps[+A](val self: PatternMatchCase[A]) extends AnyVal {
  @inline def patternExpr: Pattern[A] = self._1
  @inline def valueExpr: Value[A]     = self._2

  def mapAttributes[B](f: A => B): PatternMatchCase[B] =
    PatternMatchCase(patternExpr.mapAttributes(f), valueExpr.mapAttributes(f))
}
