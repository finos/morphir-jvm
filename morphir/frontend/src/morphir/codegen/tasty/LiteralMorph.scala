package morphir.codegen.tasty

import dotty.tools.dotc.ast.Trees.Literal
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts
import morphir.codegen.tasty.MorphUtils.*
import morphir.ir.{Value, Literal as MorphLiteral, Type as MorphType}

import scala.quoted.Quotes
import scala.util.{Failure, Try}

object LiteralMorph {
  def toValue(lit: Literal[?])(using Quotes)(using Contexts.Context): Try[Value.Value.Literal[Unit, MorphType.Type[Unit]]] = {
    lit match {
      case Literal(Constant(value: Int)) =>
        for {
          morphirType <- value.getClass.getSimpleName.toType
        } yield
          Value.Value.Literal(
            morphirType,
            MorphLiteral.wholeNumberLiteral(value)
          )

      case Literal(Constant(value: Float)) =>
        for {
          morphirType <- value.getClass.getSimpleName.toType
        } yield
          Value.Value.Literal(
            morphirType,
            MorphLiteral.floatLiteral(value)
          )

      case Literal(Constant(value: String)) =>
        for {
          morphirType <- value.getClass.getSimpleName.toType
        } yield
          Value.Value.Literal(
            morphirType,
            MorphLiteral.stringLiteral(value)
          )

      case Literal(Constant(x)) => Failure(Exception(s"Literal not supported: ${x.getClass}"))
    }
  }
}
