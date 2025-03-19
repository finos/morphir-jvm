package morphir.codegen.tasty

import dotty.tools.dotc.ast.Trees.Ident
import dotty.tools.dotc.core.Contexts
import morphir.codegen.tasty.MorphUtils.*
import morphir.ir.{Name, Path, Value, Type as MorphType}

import scala.quoted.*
import scala.util.{Success, Try}

object IdentMorph {

  def toPath(id: Ident[?])(using Quotes)(using Contexts.Context): Try[Path.Path] = {
    val path = resolveNamespace(id.symbol).reverse
    Success(Path.fromList(path))
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
