package morphir.codegen.tasty

import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.core.{ Contexts, Symbols }
import morphir.ir.{ Type as MorphType, * }
import morphir.sdk.List as MorphList

import scala.quoted.Quotes
import scala.util.{ Failure, Success, Try }

object MorphUtils {

  extension (id: Ident[?])(using Quotes)(using Contexts.Context) def toPath: Try[Path.Path] = IdentMorph.toPath(id)

  extension (id: Ident[?])(using Quotes)(using Contexts.Context) def toFQN: Try[FQName.FQName] = IdentMorph.toFQN(id)

  extension (id: Ident[?])(using Quotes)(using Contexts.Context)
    def toValue(
      inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
    ): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
      IdentMorph.toValue(id, inferredGenericTypeArgs)

  extension (t: dotty.tools.dotc.ast.Trees.Tree[?])(using Quotes)(using Contexts.Context)
    def toVersionedDistribution: Try[FormatVersion.VersionedDistribution] =
      TreeMorph.toVersionedDistribution(t)

  extension (td: TypeDef[?])(using Quotes)(using Contexts.Context)
    def toModule
      : Try[(Module.ModuleName, AccessControlled.AccessControlled[Module.Definition[Unit, MorphType.Type[Unit]]])] =
      TypeDefMorph.toModule(td)

  extension (dd: DefDef[?])(using Quotes)(using Contexts.Context)
    def toValue: Try[
      (
        Name.Name,
        AccessControlled.AccessControlled[Documented.Documented[Value.Definition[Unit, MorphType.Type[Unit]]]]
      )
    ] =
      DefDefMorph.toValue(dd)

  extension (apl: Apply[?])(using Quotes)(using Contexts.Context)
    def toValue(
      inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
    ): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
      ApplyMorph.toValue(apl, inferredGenericTypeArgs)

  extension (lit: Literal[?])(using Quotes)(using Contexts.Context)
    def toValue: Try[Value.Value[Unit, MorphType.Type[Unit]]] =
      LiteralMorph.toValue(lit)

  extension (sel: Select[?])(using Quotes)(using Contexts.Context)
    def toValue(
      inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
    ): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
      SelectMorph.toValue(sel, inferredGenericTypeArgs)

  extension (ifs: If[?])(using Quotes)(using Contexts.Context)
    def toValue(
      inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
    ): Try[Value.Value.IfThenElse[Unit, MorphType.Type[Unit]]] =
      IfMorph.toValue(ifs, inferredGenericTypeArgs)

  extension (mtch: Match[?])(using Quotes)(using Contexts.Context)
    def toValue(
      inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
    ): Try[Value.Value.PatternMatch[Unit, MorphType.Type[Unit]]] =
      MatchMorph.toValue(mtch, inferredGenericTypeArgs)

  // ** Utility functions **

  def resolveNamespace(symbol: Symbols.Symbol)(using Quotes)(using Contexts.Context): List[String] =
    if (symbol.isRoot) List()
    else symbol.name.show +: resolveNamespace(symbol.maybeOwner)

  extension (value: Value.Value[Unit, MorphType.Type[Unit]])
    def extractType: Try[MorphType.Type[Unit]] = value match {
      case Value.Value.Literal(t, _)             => Success(t)
      case Value.Value.Variable(t, _)            => Success(t)
      case Value.Value.Apply(t, _, _)            => Success(t)
      case Value.Value.Reference(t, _)           => Success(t)
      case Value.Value.Constructor(t, _)         => Success(t)
      case Value.Value.Field(t, _, _)            => Success(t)
      case Value.Value.Lambda(t, _, _)           => Success(t)
      case Value.Value.IfThenElse(t, _, _, _)    => Success(t)
      case Value.Value.LetDefinition(t, _, _, _) => Success(t)
      case Value.Value.PatternMatch(t, _, _)     => Success(t)
      case Value.Value.Tuple(t, _)               => Success(t)
      case Value.Value.List(t, _)                => Success(t)
      case Value.Value.Unit(t)                   => Success(t)
      case x                                     => Failure(UnsupportedOperationException(s"Value type is not supported: ${x.getClass}"))
    }

  // ** Keeping Scala utilities here as well as long as there are only a few of them **

  extension [A](listOfTries: List[Try[A]])
    def toTryList: Try[List[A]] = listOfTries.collectFirst { case Failure(exc) =>
      Failure(exc)
    } getOrElse {
      Success(listOfTries.collect { case Success(value) =>
        value
      })
    }
}
