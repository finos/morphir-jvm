package morphir.ir.syntax

import morphir.ir.RecordFields
import scala.language.implicitConversions

trait RecordFieldsSyntax {
  final implicit def morphirSyntaxRecordFields[A](fields: RecordFields[A]): RecordFieldsOps[A] =
    new RecordFieldsOps[A](fields)
}

final class RecordFieldsOps[+A](val self: RecordFields[A]) extends AnyVal {
  import RecordFieldSyntax._
  def mapAttributes[B](f: A => B): RecordFields[B] = self.map(_.mapAttributes(f))
}
