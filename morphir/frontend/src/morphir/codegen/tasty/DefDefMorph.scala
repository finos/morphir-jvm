package morphir.codegen.tasty

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.core.{ Contexts, Flags, Names, Types }
import morphir.codegen.tasty.MorphUtils.*
import morphir.ir.{ AccessControlled, Documented, Name, Value, Type as MorphType }
import morphir.sdk.List as MorphList

import scala.quoted.Quotes
import scala.util.{ Failure, Try }

object DefDefMorph extends TreeResolver {
  private val selfParamName = Name.fromString("this")

  def toValue(dd: Trees.DefDef[?])(using Quotes)(using Contexts.Context): Try[
    (Name.Name, AccessControlled.AccessControlled[Documented.Documented[Value.Definition[Unit, MorphType.Type[Unit]]]])
  ] =
    dd match {
      case Trees.DefDef(
            methodName,
            valDefs: List[List[?]] @unchecked,
            outputTypeId: Trees.Ident[?],
            preRhs: Trees.Tree[?]
          ) =>
        toMethodDefinition(methodName, valDefs, outputTypeId.typeOpt, preRhs)

      case Trees.DefDef(methodName, valDefs: List[List[?]] @unchecked, outputType, preRhs: Trees.Tree[?]) =>
        toMethodDefinition(methodName, valDefs, outputType.typeOpt, preRhs)

      case x => Failure(Exception(s"DefDef type not supported: ${x.getClass}"))
    }

  private def toMethodDefinition(
    methodName: Names.TermName,
    valDefs: List[List[?]],
    outputTypeOpt: Types.Type,
    preRhs: Trees.Tree[?]
  )(using Quotes)(using Contexts.Context): Try[
    (Name.Name, AccessControlled.AccessControlled[Documented.Documented[Value.Definition[Unit, MorphType.Type[Unit]]]])
  ] =
    for {
      inputTypes <- getInputTypes(valDefs, isCaseMethod = false, ownerType = None)
      outputType <- outputTypeOpt.toType(inferredGenericTypeArgs =
                      None
                    ) // Generic arguments or return type is expected to be defined on a method
      maybeGenericTypeArgs = outputType.extractGenericTypeArgs
      body                <- expandSubTree(preRhs, maybeGenericTypeArgs)
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

  def toCaseMethodValue(dd: Trees.DefDef[?], ownerType: MorphType.Type[Unit])(using
    Quotes
  )(using Contexts.Context): Try[
    (Name.Name, AccessControlled.AccessControlled[Documented.Documented[Value.Definition[Unit, MorphType.Type[Unit]]]])
  ] =
    dd match {
      case Trees.DefDef(
            methodName,
            valDefs: List[List[?]] @unchecked,
            outputTypeId: Trees.Ident[?],
            preRhs: Trees.Tree[?]
          ) =>
        toCaseMethodDefinition(methodName, valDefs, outputTypeId.typeOpt, preRhs, ownerType)

      case Trees.DefDef(methodName, valDefs: List[List[?]] @unchecked, outputType, preRhs: Trees.Tree[?]) =>
        toCaseMethodDefinition(methodName, valDefs, outputType.typeOpt, preRhs, ownerType)

      case x => Failure(Exception(s"DefDef type not supported: ${x.getClass}"))
    }

  private def toCaseMethodDefinition(
    methodName: Names.TermName,
    valDefs: List[List[?]],
    outputTypeOpt: Types.Type,
    preRhs: Trees.Tree[?],
    ownerType: MorphType.Type[Unit]
  )(using Quotes)(using Contexts.Context): Try[
    (Name.Name, AccessControlled.AccessControlled[Documented.Documented[Value.Definition[Unit, MorphType.Type[Unit]]]])
  ] =
    for {
      inputTypes          <- getInputTypes(valDefs, isCaseMethod = true, ownerType = Some(ownerType))
      outputType          <- outputTypeOpt.toType(inferredGenericTypeArgs = None)
      maybeGenericTypeArgs = outputType.extractGenericTypeArgs
      body                <- expandSubTree(preRhs, maybeGenericTypeArgs)
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

  private def getInputTypes(
    valDefs: List[List[?]],
    isCaseMethod: Boolean,
    ownerType: Option[MorphType.Type[Unit]]
  )(using
    Quotes
  )(using Contexts.Context): Try[MorphList.List[(Name.Name, MorphType.Type[Unit], MorphType.Type[Unit])]] = {
    val listOfTries: MorphList.List[Try[(Name.Name, MorphType.Type[Unit], MorphType.Type[Unit])]] =
      valDefs.flatten.map { case Trees.ValDef(paramName, tree: Trees.Tree[?], preRhs) =>
        for {
          morphType <- resolveType(tree, inferredGenericTypeArgs = None)
        } yield (Name.fromString(paramName.show), morphType, morphType)
      }

    listOfTries.toTryList.flatMap { params =>
      if isCaseMethod && params.size > 2 then
        Failure(NotImplementedError("Case-class methods with more than two explicit parameters are not supported"))
      else {
        val maybeSelfParam =
          ownerType.filter(_ => isCaseMethod).map(owner => (selfParamName, owner, owner))

        Try(maybeSelfParam.toList ++ params)
      }
    }
  }
}
