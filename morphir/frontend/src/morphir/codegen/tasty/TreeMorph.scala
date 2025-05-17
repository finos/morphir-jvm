package morphir.codegen.tasty

import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.core.Contexts
import morphir.codegen.tasty.MorphUtils.*
import morphir.ir.{Type as MorphType, *}

import scala.quoted.*
import scala.util.{Failure, Success, Try}

object TreeMorph {

  def toVersionedDistribution(tree: Tree[?])(using Quotes)(using Contexts.Context): Try[FormatVersion.VersionedDistribution] = {
    tree match {
      case PackageDef(packageIdent: Ident[?], stats) =>
        for {
          packageId <- packageIdent.toPath
          modules <- getModules(stats)
        } yield {
          FormatVersion.VersionedDistribution(
            formatVersion = 3,
            distribution = Distribution.Distribution.Library(
              packageId,
              Map(),
              modules
            )
          )
        }

      case _ =>
        Failure(Exception("Package object could not be found"))
    }
  }

  private def getModules(stats: List[Tree[?]])(using Quotes)(using Contexts.Context): Try[Package.Definition[Unit, MorphType.Type[Unit]]] = {
    val maybeModules: List[Try[(Module.ModuleName, AccessControlled.AccessControlled[Module.Definition[Unit, MorphType.Type[Unit]]])]] = stats.collect {
      case td: TypeDef[?] => td.toModule
    }

    // Convert List of Try to Try of list
    maybeModules.collectFirst {
      case Failure(exc) => Failure(exc)
    } getOrElse {
      Success(
        Package.Definition(
          maybeModules.collect {
            case Success(value) => value
          }.toMap
        )
      )
    }
  }
}
