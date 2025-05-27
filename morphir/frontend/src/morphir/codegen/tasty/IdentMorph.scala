package morphir.codegen.tasty

import dotty.tools.dotc.ast.Trees.Ident
import dotty.tools.dotc.core.Contexts
import morphir.codegen.tasty.MorphUtils.*
import morphir.ir.{FQName, Name, Path, Value, Type as MorphType}

import scala.quoted.*
import scala.util.{Failure, Success, Try}

object IdentMorph {

  def toPath(id: Ident[?])(using Quotes)(using Contexts.Context): Try[Path.Path] = {
    val path = resolveNamespace(id.symbol).map(Name.fromString).reverse
    Success(Path.fromList(path))
  }

  def toFQN(id: Ident[?])(using Quotes)(using Contexts.Context): Try[FQName.FQName] = {
    resolveNamespace(id.symbol) match {
      case localName :: moduleName :: packageName =>
        Success(FQName.fqn(packageName.reverse.mkString("."))(moduleName)(localName))
      case x =>
        Failure(Exception(s"Could not resolve FQName from: $x"))
    }
  }

  def toVariable(id: Ident[?])(using Quotes)(using Contexts.Context): Try[Value.Value.Variable[Unit, MorphType.Type[Unit]]] = {
    // FIXME: check if the below 'typeSymbol.name.toType' actually works
    id.symbol.localReturnType.typeSymbol.name.toType map { typeRef =>
      Value.Value.Variable(
        typeRef,
        Name.fromString(id.symbol.name.show)
      )
    }
  }
}
