package morphir.codegen.tasty

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.ast.Trees.Select
import dotty.tools.dotc.core.{ Contexts, Flags }
import morphir.codegen.tasty.MorphUtils.*
import morphir.ir.{ FQName, Name, Value, Type as MorphType }
import morphir.sdk.List as MorphList

import scala.quoted.Quotes
import scala.util.{ Failure, Success, Try }

object SelectMorph extends TreeResolver {
  private val selfParamName = Name.fromString("this")

  def toValue(sel: Select[?], inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]])(using
    Quotes
  )(using Contexts.Context): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
    sel match {
      case Select(_, _) if isUnitValue(sel) =>
        Success(toUnitValue)

      case Select(_, _) if isEnumConstructor(sel) =>
        for {
          returnType  <- resolveType(sel, inferredGenericTypeArgs)
          constructor <- toConstructorFQName(sel.symbol)
        } yield Value.Value.Constructor(
          returnType,
          constructor
        )

      case Select(qualifier: Trees.This[?], fieldName) if sel.symbol.flags.is(Flags.CaseAccessor) =>
        for {
          subjectType <- resolveType(qualifier, inferredGenericTypeArgs = None)
          returnType <-
            sel.symbol.denot.info.resultType.toType(subjectType.extractGenericTypeArgs.orElse(inferredGenericTypeArgs))
        } yield Value.Value.Field(
          returnType,
          Value.Value.Variable(subjectType, selfParamName),
          morphir.ir.Name.fromString(fieldName.show)
        )

      case Select(qualifier, fieldName) if sel.symbol.flags.is(Flags.CaseAccessor) =>
        for {
          subject             <- expandSubTree(qualifier, inferredGenericTypeArgs = None)
          maybeGenericTypeArgs = subject.extractType.toOption.flatMap(_.extractGenericTypeArgs)
          returnType          <- sel.symbol.denot.info.resultType.toType(maybeGenericTypeArgs.orElse(inferredGenericTypeArgs))
        } yield Value.Value.Field(
          returnType,
          subject,
          morphir.ir.Name.fromString(fieldName.show)
        )

      case Select(qualifier, _) if isCaseMethod(sel) =>
        for {
          subject     <- expandSubTree(qualifier, inferredGenericTypeArgs = None)
          subjectType <- subject.extractType
          genericArgs  = subjectType.extractGenericTypeArgs.orElse(inferredGenericTypeArgs)
          returnType  <- sel.symbol.denot.info.resultType.toType(genericArgs)
          parameters   = sel.symbol.paramSymss.flatten
          _ <- if parameters.size <= 2 then Success(())
               else
                 Failure(
                   NotImplementedError("Case-class methods with more than two explicit parameters are not supported")
                 )
          parameterTypes <- parameters.map(_.denot.info.toType(genericArgs)).toTryList
          functionName <- resolveNamespace(sel.symbol) match {
                            case localName :: moduleName :: packageName =>
                              Success(FQName.fqn(packageName.reverse.mkString("."))(moduleName)(localName))
                            case namespace =>
                              Failure(Exception(s"Could not resolve case-method FQName from: $namespace"))
                          }
          appliedType =
            parameterTypes.foldRight(returnType)((parameter, result) => MorphType.Function((), parameter, result))
        } yield Value.Value.Apply(
          appliedType,
          Value.Value.Reference(MorphType.Function((), subjectType, appliedType), functionName),
          subject
        )

      case Select(qualifier, _) =>
        for {
          returnType          <- resolveType(sel, inferredGenericTypeArgs)
          maybeGenericTypeArgs = returnType.extractGenericTypeArgs
          argument            <- expandSubTree(qualifier, maybeGenericTypeArgs)
          argumentType        <- argument.extractType
          function            <- StandardFunctions.get(sel.symbol, returnType, argumentType)
          appliedType <- function.extractType.flatMap {
                           case MorphType.Function(_, _, nextReturnType) => Try(nextReturnType)
                           case functionType =>
                             Try(throw Exception(s"Select did not resolve to an applicable function: $functionType"))
                         }
        } yield Value.Value.Apply(
          appliedType,
          function,
          argument
        )
    }

  private def isCaseMethod(sel: Select[?])(using Quotes)(using Contexts.Context): Boolean =
    sel.symbol.owner.flags.is(Flags.Case) &&
      sel.symbol.flags.is(Flags.Method) &&
      !sel.symbol.flags.is(Flags.Synthetic) &&
      !sel.symbol.flags.is(Flags.CaseAccessor) &&
      !List("copy", "product", "_").exists(sel.symbol.name.show.startsWith) &&
      !Set("canEqual", "equals", "hashCode", "toString", "writeReplace").contains(sel.symbol.name.show)

  private def isEnumConstructor(sel: Select[?])(using Quotes)(using Contexts.Context): Boolean =
    (sel.symbol.flags.is(Flags.Case) && !sel.symbol.flags.is(Flags.CaseAccessor)) ||
      sel.symbol.companionClass.flags.is(Flags.Case)

  private def isUnitValue(sel: Select[?])(using Quotes)(using Contexts.Context): Boolean =
    isUnitValueSymbol(resolveNamespace(sel.symbol))
}
