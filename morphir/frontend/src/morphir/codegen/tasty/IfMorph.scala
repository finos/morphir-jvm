package morphir.codegen.tasty

import dotty.tools.dotc.ast.Trees.If
import dotty.tools.dotc.core.Contexts
import morphir.ir.{ Value, Type as MorphType }
import morphir.sdk.List as MorphList

import scala.quoted.Quotes
import scala.util.Try

object IfMorph extends TreeResolver {

  def toValue(ifs: If[?], inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]])(using
    Quotes
  )(using Contexts.Context): Try[Value.Value.IfThenElse[Unit, MorphType.Type[Unit]]] =
    for {
      returnType          <- resolveType(ifs, inferredGenericTypeArgs)
      maybeGenericTypeArgs = returnType.extractGenericTypeArgs
      condition           <- expandSubTree(ifs.cond, maybeGenericTypeArgs)
      thenBranch          <- expandSubTree(ifs.thenp, maybeGenericTypeArgs)
      elseBranch          <- expandSubTree(ifs.elsep, maybeGenericTypeArgs)
    } yield Value.Value.IfThenElse(
      returnType,
      condition,
      thenBranch,
      elseBranch
    )
}
