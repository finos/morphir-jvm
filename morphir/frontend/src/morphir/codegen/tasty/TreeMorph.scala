package morphir.codegen.tasty

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.core.Contexts
import morphir.codegen.tasty.MorphUtils.*
import morphir.ir.{ Type as MorphType, * }
import morphir.sdk.Dict

import scala.quoted.*
import scala.util.{ Failure, Try }

object TreeMorph {

  def toVersionedDistribution(
    tree: Trees.Tree[?]
  )(using Quotes)(using Contexts.Context): Try[FormatVersion.VersionedDistribution] =
    tree match {
      case Trees.PackageDef(packageIdent: Trees.Ident[?], stats) =>
        for {
          packageId <- packageIdent.toPath
          modules   <- getModules(stats)
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

  private def getModules(
    stats: List[Trees.Tree[?]]
  )(using Quotes)(using Contexts.Context): Try[Package.Definition[Unit, MorphType.Type[Unit]]] = {
    val listOfTries: List[
      Try[(Module.ModuleName, AccessControlled.AccessControlled[Module.Definition[Unit, MorphType.Type[Unit]]])]
    ] =
      stats.collect {
        case td: Trees.TypeDef[?] if !TypeDefMorph.shouldIgnore(td) => td.toModule
      }

    listOfTries.toTryList.map { modules =>
      Package.Definition(
        modules = modules.foldLeft(
          Dict
            .empty[Module.ModuleName, AccessControlled.AccessControlled[Module.Definition[Unit, MorphType.Type[Unit]]]]
        ) { case (acc, (moduleName, moduleDef)) =>
          acc.get(moduleName) match {
            case Some(existing) =>
              acc.updated(moduleName, mergeModuleDefinitions(existing, moduleDef))
            case None =>
              acc.updated(moduleName, moduleDef)
          }
        }
      )
    }
  }

  private def mergeModuleDefinitions(
    left: AccessControlled.AccessControlled[Module.Definition[Unit, MorphType.Type[Unit]]],
    right: AccessControlled.AccessControlled[Module.Definition[Unit, MorphType.Type[Unit]]]
  ): AccessControlled.AccessControlled[Module.Definition[Unit, MorphType.Type[Unit]]] = {
    val mergedAccess =
      if (left.access == AccessControlled.Access.Public || right.access == AccessControlled.Access.Public)
        AccessControlled.Access.Public
      else AccessControlled.Access.Private

    AccessControlled.AccessControlled(
      access = mergedAccess,
      value = Module.Definition(
        types = left.value.types ++ right.value.types,
        values = left.value.values ++ right.value.values,
        doc = left.value.doc.orElse(right.value.doc)
      )
    )
  }
}
