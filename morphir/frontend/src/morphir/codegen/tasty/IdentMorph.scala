package morphir.codegen.tasty

import dotty.tools.dotc.ast.Trees.Ident
import dotty.tools.dotc.core.{ Contexts, Flags, Symbols, Types }
import morphir.codegen.tasty.MorphUtils.*
import morphir.ir.{ FQName, Name, Path, Value, Type as MorphType }
import morphir.sdk.List as MorphList

import scala.quoted.*
import scala.util.{ Failure, Success, Try }

object IdentMorph extends TreeResolver {
  private val selfParamName = Name.fromString("this")

  def toPath(id: Ident[?])(using Quotes)(using Contexts.Context): Try[Path.Path] = {
    val path = resolveNamespace(id.symbol).map(Name.fromString).reverse
    Success(Path.fromList(path))
  }

  def toFQN(id: Ident[?])(using Quotes)(using Contexts.Context): Try[FQName.FQName] =
    resolveNamespace(id.symbol) match {
      case localName :: moduleName :: packageName =>
        Success(FQName.fqn(packageName.reverse.mkString("."))(moduleName)(localName))
      case x =>
        Failure(Exception(s"Could not resolve FQName from: $x"))
    }

  def toValue(id: Ident[?], inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]] = None)(using
    Quotes
  )(using Contexts.Context): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
    if isNilValue(id) then toEmptyListValue(id, inferredGenericTypeArgs)
    else if isUnitValue(id) then Success(toUnitValue)
    else if id.symbol.flags.is(Flags.CaseAccessor) then toCaseAccessorValue(id, inferredGenericTypeArgs)
    else
      id.typeOpt match {
        case Types.TermRef(prefix, sbl: Symbols.Symbol) =>
          for {
            typeRef <- sbl.localReturnType.toType(inferredGenericTypeArgs)
          } yield Value.Value.Variable(
            typeRef,
            Name.fromString(id.symbol.name.show)
          )

        case typeOpt: Types.Type =>
          for {
            typ <- typeOpt.toType(inferredGenericTypeArgs)
            fun <- StandardFunctions.get(id.symbol, typ)
          } yield fun
      }

  private def toCaseAccessorValue(
    id: Ident[?],
    inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
  )(using Quotes)(using Contexts.Context): Try[Value.Value.Field[Unit, MorphType.Type[Unit]]] =
    for {
      subjectType <- id.symbol.owner.typeRef.toType(inferredGenericTypeArgs = None)
      returnType  <- resolveType(id, inferredGenericTypeArgs)
    } yield Value.Value.Field(
      returnType,
      Value.Value.Variable(subjectType, selfParamName),
      Name.fromString(id.symbol.name.show)
    )

  private def toEmptyListValue(
    id: Ident[?],
    inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
  )(using Quotes)(using Contexts.Context): Try[Value.Value.List[Unit, MorphType.Type[Unit]]] =
    for {
      listType <- resolveType(id, inferredGenericTypeArgs).orElse {
                    inferredGenericTypeArgs match {
                      case Some(typeArgs) if typeArgs.size == 1 => Success(StandardTypes.listReference(typeArgs))
                      case _                                    => Failure(Exception("Could not resolve empty list type for Nil"))
                    }
                  }
    } yield Value.Value.List(
      listType,
      List.empty
    )

  private def isNilValue(id: Ident[?])(using Quotes)(using Contexts.Context): Boolean =
    resolveNamespace(id.symbol) match {
      case "Nil" :: "package" :: "scala" :: Nil => true
      case "Nil" :: _                           => true
      case _                                    => false
    }

  private def isUnitValue(id: Ident[?])(using Quotes)(using Contexts.Context): Boolean =
    isUnitValueSymbol(resolveNamespace(id.symbol))
}
