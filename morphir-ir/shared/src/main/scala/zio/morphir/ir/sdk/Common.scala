package zio.morphir.ir.sdk

import zio.morphir.ir.ValueModule
import zio.morphir.ir.TypeModule.Type
import zio.morphir.ir.Name

object Common {
  final class VSpec(private val name: () => String) extends AnyVal {
    def apply[Annotations](inputs: (Name, Type[Annotations])*) =
      ValueModule.Specification.create(inputs: _*)
  }

}
