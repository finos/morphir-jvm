package morphir.codegen.tasty

import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.core.{Contexts, Names, Symbols}
import morphir.ir.{Type as MorphType, *}

import scala.quoted.Quotes
import scala.util.{Failure, Success, Try}

object MorphUtils {

  extension (id: Ident[?])(using Quotes)(using Contexts.Context)
    def toPath: Try[Path.Path] = IdentMorph.toPath(id)

  extension (id: Ident[?])(using Quotes)(using Contexts.Context)
    def toVariable: Try[Value.Value.Variable[Unit, MorphType.Type[Unit]]] = IdentMorph.toVariable(id)

  extension (t: dotty.tools.dotc.ast.Trees.Tree[?])(using Quotes)(using Contexts.Context)
    def toVersionedDistribution: Try[FormatVersion.VersionedDistribution] = TreeMorph.toVersionedDistribution(t)

  extension (td: TypeDef[?])(using Quotes)(using Contexts.Context)
    def toModule: Try[(Module.ModuleName, AccessControlled.AccessControlled[Module.Definition[Unit, MorphType.Type[Unit]]])] = TypeDefMorph.toModule(td)

  extension (dd: DefDef[?])(using Quotes)(using Contexts.Context)
    def toValue: Try[(Name.Name, AccessControlled.AccessControlled[Documented.Documented[Value.Definition[Unit, MorphType.Type[Unit]]]])] = DefDefMorph.toValue(dd)

  extension (apl: Apply[?])(using Quotes)(using Contexts.Context)
    def toValue: Try[Value.Value.Apply[Unit, MorphType.Type[Unit]]] = ApplyMorph.toValue(apl)

  extension (lit: Literal[?])(using Quotes)(using Contexts.Context)
    def toValue: Try[Value.Value.Literal[Unit, MorphType.Type[Unit]]] = LiteralMorph.toValue(lit)

  extension (sel: Select[?])(using Quotes)(using Contexts.Context)
    def toValue: Try[Value.Value.Apply[Unit, MorphType.Type[Unit]]] = SelectMorph.toValue(sel)

  // ** Utility functions **

  def resolveNamespace(symbol: Symbols.Symbol)(using Quotes)(using Contexts.Context): List[Name.Name] = {
    if (symbol.isRoot) List()
    else Name.fromString(symbol.name.show) +: resolveNamespace(symbol.maybeOwner)
  }

  extension (name: Names.Name)(using Quotes)(using Contexts.Context)
    def toType: Try[MorphType.Type[Unit]] = {
      name.show.toType
    }

  extension (name: String)
    def toType: Try[MorphType.Type[Unit]] = {
      name match {
        case "int" | "Int" | "java.lang.Integer" =>
          Success(StandardTypes.intReference)
        case "float" | "Float" | "java.lang.Float" =>
          Success(StandardTypes.floatReference)
        case "string" | "String" =>
          Success(StandardTypes.stringReference)
        case _ => Failure(UnsupportedOperationException(s"Type name is not supported: $name"))
      }
    }

  extension (value: Value.Value[Unit, MorphType.Type[Unit]])
    def getType: Try[MorphType.Type[Unit]] = value match {
      case Value.Value.Literal(t, _) => Success(t)
      case Value.Value.Variable(t, _) => Success(t)
      case Value.Value.Apply(t, _, _) => Success(t)
      case x => Failure(UnsupportedOperationException(s"Value type is not supported: ${x.getClass}"))
    }

  // ** Keeping Scala utilities here as well as long as there are only a few of them **

  extension [A](listOfTries: List[Try[A]])
    def toTryList: Try[List[A]] = listOfTries.collectFirst {
      case Failure(exc) => Failure(exc)
    } getOrElse {
      Success(listOfTries.collect {
        case Success(value) => value
      })
    }
}
