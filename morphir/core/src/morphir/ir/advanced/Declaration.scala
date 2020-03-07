package morphir.ir.advanced
import morphir.ir.Name

case class Declaration[X](inputs: List[(Name, Type[X])], output: Type[X])
