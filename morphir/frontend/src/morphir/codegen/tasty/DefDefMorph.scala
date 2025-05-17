package morphir.codegen.tasty

import dotty.tools.dotc.ast.Trees.{Apply, DefDef, Ident, ValDef}
import dotty.tools.dotc.core.{Contexts, Names}
import morphir.codegen.tasty.MorphUtils.*
import morphir.ir.{AccessControlled, Documented, Name, Value, Type as MorphType}
import morphir.sdk.List as MorphList

import scala.quoted.Quotes
import scala.util.{Failure, Try}

object DefDefMorph {

  def toValue(dd: DefDef[?])(using Quotes)(using Contexts.Context): Try[(Name.Name, AccessControlled.AccessControlled[Documented.Documented[Value.Definition[Unit, MorphType.Type[Unit]]]])] = {
    dd match {
      case DefDef(methodName, valDefs: List[List[?]] @unchecked, Ident(outputTypeName), preRhs) =>
        toMethodDefinition(methodName, valDefs, outputTypeName, preRhs)

      case DefDef(methodName, valDefs: List[List[?]] @unchecked, tree, preRhs) =>
        val outputTypeName = tree.typeOpt.typeSymbol.name
        toMethodDefinition(methodName, valDefs, outputTypeName, preRhs)
    }
  }

  private def toMethodDefinition(methodName: Names.TermName, valDefs: List[List[?]], outputTypeName: Names.Name, preRhs: AnyRef)(using Quotes)(using Contexts.Context): Try[(Name.Name, AccessControlled.AccessControlled[Documented.Documented[Value.Definition[Unit, MorphType.Type[Unit]]]])] = {
    for {
      inputTypes <- getInputTypes(valDefs)
      outputType <- outputTypeName.toType
      body <- getBody(preRhs)
    } yield {
      val valueDef = Value.Definition(
        inputTypes = inputTypes,
        outputType = outputType,
        body = body
      )

      val valueDoc = Documented.Documented(
        doc = "",
        value = valueDef
      )

      val valueAccessControlled = AccessControlled.AccessControlled(
        access = AccessControlled.Access.Public,
        value = valueDoc
      )

      (Name.fromString(methodName.show), valueAccessControlled)
    }
  }

  private def getInputTypes(valDefs: List[List[?]])(using Quotes)(using Contexts.Context): Try[MorphList.List[(Name.Name, MorphType.Type[Unit], MorphType.Type[Unit])]] = {
    val listOfTries: MorphList.List[Try[(Name.Name, MorphType.Type[Unit], MorphType.Type[Unit])]] =
      valDefs.flatten.map { case ValDef(paramName, Ident(typeName), preRhs) =>
        for {
          morphType <- typeName.toType
        } yield (Name.fromString(paramName.show), morphType, morphType)
      }

    listOfTries.toTryList
  }

  private def getBody(preRhs: AnyRef)(using Quotes)(using Contexts.Context): Try[Value.Value[Unit, MorphType.Type[Unit]]] = {
    preRhs match {
      case apl: Apply[?] => apl.toValue
      case ident: Ident[?] => ident.toVariable
      case x => Failure(Exception(s"DefDef body type not supported: ${x.getClass}"))
    }
  }
}
