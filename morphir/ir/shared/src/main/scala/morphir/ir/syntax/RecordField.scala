package morphir.ir.syntax

import morphir.ir.{ Name, RecordField, Value }
import scala.language.implicitConversions

trait RecordFieldSyntax {
  final implicit def morphirSyntaxRecordField[A](field: RecordField[A]): RecordFieldOps[A] = new RecordFieldOps(field)
}

object RecordFieldSyntax extends RecordFieldSyntax

final class RecordFieldOps[+A](val self: RecordField[A]) extends AnyVal {
  @inline def fieldName: Name      = self._1
  @inline def fieldValue: Value[A] = self._2

  def mapAttributes[B](f: A => B): RecordField[B] = self.copy(_2 = fieldValue.mapAttributes(f))
}
