package zio.morphir.syntax

import zio.morphir.ir.value.recursive.AllValueSyntax

trait ValueSyntax extends AllValueSyntax {
  final val define = zio.morphir.syntax.define
}
