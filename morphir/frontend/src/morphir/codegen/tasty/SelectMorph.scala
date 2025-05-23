package morphir.codegen.tasty

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.Trees.{Apply, Ident, Literal, Select}
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Types.{ConstantType, TermRef, TypeRef}
import dotty.tools.dotc.core.{Contexts, Names}
import morphir.codegen.tasty.MorphUtils.*
import morphir.ir.{Value, Type as MorphType}

import scala.quoted.Quotes
import scala.util.{Failure, Try}

object SelectMorph {

  def toValue(sel: Select[?])(using Quotes)(using Contexts.Context): Try[Value.Value.Apply[Unit, MorphType.Type[Unit]]] = {
    sel match {
      case Select(qualifier, name) =>
        for {
          argument <- getFunctionArgument(qualifier)
          argumentType <- argument.getType
          returnType <- getReturnType(sel)
          function <- StandardFunctions.get(name, returnType, argumentType)
        } yield
          Value.Value.Apply(
            MorphType.Function(
              (),
              argumentType,
              returnType
            ),
            function,
            argument
          )
    }
  }

  private def getFunctionArgument(qualifier: Trees.Tree[?])(using Quotes)(using Contexts.Context): Try[Value.Value[Unit, MorphType.Type[Unit]]] = {
    qualifier match {
      case apl: Apply[?] => apl.toValue
      case ident: Ident[?] => ident.toVariable
      case lit: Literal[?] => lit.toValue
      case x => Failure(Exception(s"Qualifier not supported: ${x.getClass}"))
    }
  }

  private def getReturnType(sel: Select[?])(using Quotes)(using Contexts.Context): Try[MorphType.Type[Unit]] = {
    // FIXME: check that the below conversions produce the expected outcome
    sel.typeOpt match {
      case TypeRef(_, sbl: Symbol) => sbl.name.toType
      case ConstantType(const) => const.tpe.typeSymbol.name.toType
      case TermRef(_, sbl: Symbol) => sbl.denot.info.resultType.typeSymbol.name.toType
      case x => Failure(Exception(s"Return type not supported: ${x.getClass}"))
    }
  }
}
