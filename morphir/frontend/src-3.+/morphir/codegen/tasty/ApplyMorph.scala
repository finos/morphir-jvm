package morphir.codegen.tasty

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.Trees.{Apply, Ident, Literal, Select}
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.{ConstantType, TypeRef}
import dotty.tools.dotc.core.{Contexts, Names}
import morphir.codegen.tasty.MorphUtils.*
import morphir.ir.{Value, Type as MorphType}

import scala.quoted.Quotes
import scala.util.{Failure, Try}

object ApplyMorph {

  def toValue(apl: Apply[?])(using Quotes)(using Contexts.Context): Try[Value.Value.Apply[Unit, MorphType.Type[Unit]]] = {
    apl match {
      case Apply(sel@Select(qualifier, name), args) =>
        for {
          argument <- getFunctionArgument(args)
          returnType <- getReturnType(apl)
          function <- sel.toValue
        } yield
          Value.Value.Apply(
            returnType,
            function,
            argument
          )

      case x => Failure(Exception(s"Apply could not be processed: ${x.getClass}"))
    }
  }

  private def getReturnType(apl: Apply[?])(using Quotes)(using Contexts.Context): Try[MorphType.Type[Unit]] = {
    // FIXME: check that the below conversions produce the expected outcome
    apl.typeOpt match {
      case TypeRef(_, sbl: Symbol) => sbl.name.toType
      case ConstantType(const) => const.tpe.typeSymbol.name.toType
      case x => Failure(Exception(s"Return type not supported: ${x.getClass}"))
    }
  }

  private def getFunctionArgument(args: List[Trees.Tree[?]])(using Quotes)(using Contexts.Context): Try[Value.Value[Unit, MorphType.Type[Unit]]] = {
    Try(args)
      .map {
        case oneElem :: Nil => oneElem
        case moreElem => throw Exception(s"Number of args: ${moreElem.size}, but only one is supported")
      }
      .map {
        case apl: Apply[?] => apl.toValue
        case ident: Ident[?] => ident.toVariable
        case lit: Literal[?] => lit.toValue
        case x => Failure(Exception(s"Variable type not supported: ${x.getClass}"))
      }
      .flatten
  }
}
