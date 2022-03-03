package zio.morphir
import scalafix.lint.Diagnostic
import zio.prelude.*
package object fix {
  type Result[+A] = Validation[Diagnostic, A]
  val Result = Validation
}
