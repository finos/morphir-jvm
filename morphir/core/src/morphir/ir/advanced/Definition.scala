package morphir.ir.advanced
import morphir.ir.Name

sealed abstract class Definition[X]
object Definition {
  case class TypedDefinition[X](
      valueType: Type[X],
      argumentNames: List[Name],
      body: Value[X]
  ) extends Definition[X]

  case class UntypedDefinition[X](argumentNames: List[Name], body: Value[X])
      extends Definition[X]
}
