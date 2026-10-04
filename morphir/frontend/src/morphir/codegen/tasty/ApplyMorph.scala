package morphir.codegen.tasty

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.core.{ Contexts, Flags, Names }
import morphir.codegen.tasty.MorphUtils.*
import morphir.ir.{ Value, Type as MorphType }
import morphir.sdk.List as MorphList

import scala.quoted.Quotes
import scala.util.{ Failure, Success, Try }

object ApplyMorph extends TreeResolver {

  def toValue(apl: Trees.Apply[?], inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]])(using
    Quotes
  )(using Contexts.Context): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
    apl match {
      case Trees.Apply(sel: Trees.Select[?], args) =>
        if isEnumConstructorApply(sel) then
          for {
            returnType <- resolveType(apl, inferredGenericTypeArgs)
            function   <- toEnumConstructorValue(sel, args, returnType, inferredGenericTypeArgs)
            applied    <- applyArguments(function, args, returnType.extractGenericTypeArgs)
          } yield applied
        else if isMapMethodApply(sel, "get") then toMapGetValue(apl, sel, args, inferredGenericTypeArgs)
        else if isListHigherOrderMethodApply(sel, "map") || isListHigherOrderMethodApply(
            sel,
            "filter"
          ) || isListHigherOrderMethodApply(sel, "flatMap")
        then toListHigherOrderMethodValue(apl, sel, args, inferredGenericTypeArgs)
        else if isListHigherOrderMethodApply(sel, "collect") then
          toListCollectValue(apl, sel, args, inferredGenericTypeArgs)
        else
          for {
            returnType <- resolveType(apl, inferredGenericTypeArgs)
            function   <- sel.toValue(returnType.extractGenericTypeArgs)
            applied    <- applyArguments(function, args, returnType.extractGenericTypeArgs)
          } yield applied

      case Trees.Apply(functionId: Trees.Ident[?], args) =>
        for {
          returnType          <- resolveType(apl, inferredGenericTypeArgs)
          maybeGenericTypeArgs = returnType.extractGenericTypeArgs
          theApply            <- toNamedApplyValue(functionId, returnType, args, maybeGenericTypeArgs)
        } yield theApply

      case Trees.Apply(fun: Trees.TypeApply[?], args) if isTupleApply(fun, args.size) =>
        for {
          returnType <- resolveType(apl, inferredGenericTypeArgs)
          elements   <- args.map(expandSubTree(_, inferredGenericTypeArgs)).toTryList
        } yield Value.Value.Tuple(
          returnType,
          elements
        )

      case Trees.Apply(fun: Trees.TypeApply[?], args) if isListLikeApply(fun) && hasListElements(args) =>
        for {
          returnType <- resolveType(apl, inferredGenericTypeArgs).orElse {
                          inferredGenericTypeArgs match {
                            case Some(typeArgs) if typeArgs.size == 1 => Try(StandardTypes.listReference(typeArgs))
                            case _                                    => Failure(Exception("Could not resolve collection type for List/Seq apply"))
                          }
                        }
          elements <-
            extractListElements(args)
              .map(_.map(expandSubTree(_, returnType.extractGenericTypeArgs.orElse(inferredGenericTypeArgs))).toTryList)
              .getOrElse(Failure(Exception("Could not extract collection elements from List/Seq apply")))
        } yield Value.Value.List(
          returnType,
          elements
        )

      case Trees.Apply(fun: Trees.TypeApply[?], args)
          if isMapLiteralApply(apl, fun, inferredGenericTypeArgs) && hasListElements(args) =>
        toMapLiteralValue(apl, args, inferredGenericTypeArgs)

      case Trees.Apply(fun: Trees.TypeApply[?], args) if isListHigherOrderMethodApply(fun, "map") =>
        toListHigherOrderMethodValue(apl, fun, args, inferredGenericTypeArgs)

      case Trees.Apply(fun: Trees.TypeApply[?], args) if isListHigherOrderMethodApply(fun, "filter") =>
        toListHigherOrderMethodValue(apl, fun, args, inferredGenericTypeArgs)

      case Trees.Apply(fun: Trees.TypeApply[?], args) if isListHigherOrderMethodApply(fun, "flatMap") =>
        toListHigherOrderMethodValue(apl, fun, args, inferredGenericTypeArgs)

      case Trees.Apply(fun: Trees.TypeApply[?], args) if isListHigherOrderMethodApply(fun, "collect") =>
        toListCollectValue(apl, fun, args, inferredGenericTypeArgs)

      case Trees.Apply(fun: Trees.Apply[?], args) if isListFoldLeftApply(fun) =>
        toListFoldLeftValue(apl, fun, args, inferredGenericTypeArgs)

      case Trees.Apply(fun: Trees.TypeApply[?], args) =>
        for {
          returnType          <- resolveType(apl, inferredGenericTypeArgs)
          maybeGenericTypeArgs = returnType.extractGenericTypeArgs
          argument            <- getFunctionArgument(args, maybeGenericTypeArgs)
          function            <- toValue(fun, inferredGenericTypeArgs)
        } yield Value.Value.Apply(
          returnType,
          function,
          argument
        )

      case Trees.Apply(fun, args) =>
        for {
          function <- expandSubTree(fun, inferredGenericTypeArgs)
          applied  <- applyArguments(function, args, inferredGenericTypeArgs)
        } yield applied
    }

  private def toListHigherOrderMethodValue(
    apl: Trees.Apply[?],
    fun: Trees.TypeApply[?],
    args: List[Trees.Tree[?]],
    inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
  )(using Quotes)(using Contexts.Context): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
    fun match {
      case Trees.TypeApply(sel @ Trees.Select(qualifier, _), _) =>
        toListHigherOrderMethodValue(apl, sel, args, inferredGenericTypeArgs)

      case x =>
        Failure(Exception(s"List higher-order method could not be processed from: ${x.getClass}"))
    }

  private def toListCollectValue(
    apl: Trees.Apply[?],
    fun: Trees.TypeApply[?],
    args: List[Trees.Tree[?]],
    inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
  )(using Quotes)(using Contexts.Context): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
    fun match {
      case Trees.TypeApply(sel @ Trees.Select(_, _), _) =>
        toListCollectValue(apl, sel, args, inferredGenericTypeArgs)
      case x =>
        Failure(Exception(s"List collect could not be processed from: ${x.getClass}"))
    }

  private def toListCollectValue(
    apl: Trees.Apply[?],
    sel: Trees.Select[?],
    args: List[Trees.Tree[?]],
    inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
  )(using Quotes)(using Contexts.Context): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
    sel match {
      case Trees.Select(qualifier, _) =>
        for {
          returnType           <- resolveType(apl, inferredGenericTypeArgs)
          functionArgument     <- getFunctionArgument(args, inferredGenericTypeArgs).flatMap(toFilterMapLambda)
          functionArgumentType <- functionArgument.extractType
          listValue            <- expandSubTree(qualifier, inferredGenericTypeArgs = None)
          listType             <- listValue.extractType
          function <-
            StandardFunctions.getCollectionMethod(sel.symbol, returnType, List(functionArgumentType, listType))
          partiallyAppliedType <- function.extractType.flatMap {
                                    case MorphType.Function(_, _, nextReturnType) => Try(nextReturnType)
                                    case other =>
                                      Failure(
                                        Exception(
                                          s"Collection method did not resolve to an applicable function type: $other"
                                        )
                                      )
                                  }
          partiallyApplied = Value.Value.Apply(partiallyAppliedType, function, functionArgument)
        } yield Value.Value.Apply(
          returnType,
          partiallyApplied,
          listValue
        )
    }

  private def toNamedApplyValue(
    functionId: Trees.Ident[?],
    returnType: MorphType.Type[Unit],
    args: List[Trees.Tree[?]],
    inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
  )(using Quotes)(using Contexts.Context): Try[Value.Value[Unit, MorphType.Type[Unit]]] = {
    val maybeStandardFunction =
      args match {
        case firstArg :: _ =>
          for {
            firstArgument     <- expandSubTree(firstArg, inferredGenericTypeArgs)
            firstArgumentType <- firstArgument.extractType
            function          <- StandardFunctions.get(functionId.symbol, returnType, firstArgumentType)
            applied           <- applyArguments(function, args, inferredGenericTypeArgs)
          } yield applied
        case Nil =>
          Failure(Exception(s"Named apply without arguments is unsupported for ${functionId.symbol.name.show}"))
      }

    maybeStandardFunction.orElse {
      toValue(functionId, returnType, args.reverse, inferredGenericTypeArgs)
    }
  }

  private def toListHigherOrderMethodValue(
    apl: Trees.Apply[?],
    sel: Trees.Select[?],
    args: List[Trees.Tree[?]],
    inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
  )(using Quotes)(using Contexts.Context): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
    sel match {
      case Trees.Select(qualifier, _) =>
        for {
          returnType           <- resolveType(apl, inferredGenericTypeArgs)
          functionArgument     <- getFunctionArgument(args, inferredGenericTypeArgs)
          functionArgumentType <- functionArgument.extractType
          listValue            <- expandSubTree(qualifier, inferredGenericTypeArgs = None)
          listType             <- listValue.extractType
          function <-
            StandardFunctions.getCollectionMethod(sel.symbol, returnType, List(functionArgumentType, listType))
          partiallyAppliedType <- function.extractType.flatMap {
                                    case MorphType.Function(_, _, nextReturnType) => Try(nextReturnType)
                                    case other =>
                                      Failure(
                                        Exception(
                                          s"Collection method did not resolve to an applicable function type: $other"
                                        )
                                      )
                                  }
          partiallyApplied = Value.Value.Apply(partiallyAppliedType, function, functionArgument)
        } yield Value.Value.Apply(
          returnType,
          partiallyApplied,
          listValue
        )
    }

  private def toListFoldLeftValue(
    apl: Trees.Apply[?],
    fun: Trees.Apply[?],
    args: List[Trees.Tree[?]],
    inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
  )(using Quotes)(using Contexts.Context): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
    fun match {
      case Trees.Apply(sel: Trees.Select[?], initArgs) =>
        toListFoldLeftValue(apl, sel, initArgs, args, inferredGenericTypeArgs)
      case Trees.Apply(Trees.TypeApply(sel: Trees.Select[?], _), initArgs) =>
        toListFoldLeftValue(apl, sel, initArgs, args, inferredGenericTypeArgs)
      case x =>
        Failure(Exception(s"List foldLeft could not be processed from: ${x.getClass}"))
    }

  private def toListFoldLeftValue(
    apl: Trees.Apply[?],
    sel: Trees.Select[?],
    initArgs: List[Trees.Tree[?]],
    functionArgs: List[Trees.Tree[?]],
    inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
  )(using Quotes)(using Contexts.Context): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
    sel match {
      case Trees.Select(qualifier, _) =>
        for {
          returnType           <- resolveType(apl, inferredGenericTypeArgs)
          initValue            <- getFunctionArgument(initArgs, inferredGenericTypeArgs)
          initType             <- initValue.extractType
          functionArgument     <- getFunctionArgument(functionArgs, inferredGenericTypeArgs).flatMap(reorderFoldLeftLambda)
          functionArgumentType <- functionArgument.extractType
          listValue            <- expandSubTree(qualifier, inferredGenericTypeArgs = None)
          listType             <- listValue.extractType
          function <- StandardFunctions.getCollectionMethod(
                        sel.symbol,
                        returnType,
                        List(functionArgumentType, initType, listType)
                      )
          withFunction <- applyValueArgument(function, functionArgument)
          withInit     <- applyValueArgument(withFunction, initValue)
          withList     <- applyValueArgument(withInit, listValue)
        } yield withList
    }

  private def applyValueArgument(
    function: Value.Value[Unit, MorphType.Type[Unit]],
    argument: Value.Value[Unit, MorphType.Type[Unit]]
  ): Try[Value.Value.Apply[Unit, MorphType.Type[Unit]]] =
    function.extractType.flatMap {
      case MorphType.Function(_, _, returnType) =>
        Try(Value.Value.Apply(returnType, function, argument))
      case other =>
        Failure(Exception(s"Cannot apply value argument to non-function type: $other"))
    }

  private def reorderFoldLeftLambda(
    value: Value.Value[Unit, MorphType.Type[Unit]]
  ): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
    value match {
      case Value.Value.Lambda(_, accPattern, inner @ Value.Value.Lambda(_, valuePattern, body)) =>
        for {
          accType        <- patternType(accPattern)
          valueType      <- patternType(valuePattern)
          bodyType       <- body.extractType
          innerLambdaType = MorphType.Function((), accType, bodyType)
          outerLambdaType = MorphType.Function((), valueType, innerLambdaType)
        } yield Value.Value.Lambda(
          outerLambdaType,
          valuePattern,
          Value.Value.Lambda(
            innerLambdaType,
            accPattern,
            body
          )
        )
      case other =>
        Failure(Exception(s"foldLeft expects a two-argument lambda, got: ${other.getClass}"))
    }

  private def toFilterMapLambda(
    value: Value.Value[Unit, MorphType.Type[Unit]]
  ): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
    value match {
      case Value.Value.Lambda(_, argumentPattern, Value.Value.PatternMatch(_, branchOn, cases)) if cases.size == 1 =>
        for {
          outputValueType <- cases.head._2.extractType
          branchOnType    <- branchOn.extractType
          inputType       <- patternType(argumentPattern)
          maybeOutputType  = StandardTypes.maybeReference(List(outputValueType))
          justConstructor: Value.Value[Unit, MorphType.Type[Unit]] =
            Value.Value.Constructor(
              MorphType.Function((), outputValueType, maybeOutputType),
              morphir.ir.FQName.fqn("morphir.SDK")("maybe")("just")
            )
          nothingConstructor: Value.Value[Unit, MorphType.Type[Unit]] =
            Value.Value.Constructor(
              maybeOutputType,
              morphir.ir.FQName.fqn("morphir.SDK")("maybe")("nothing")
            )
          transformedCases: List[(Value.Pattern[MorphType.Type[Unit]], Value.Value[Unit, MorphType.Type[Unit]])] =
            cases.map { case (pattern, body) =>
              (pattern, Value.Value.Apply(maybeOutputType, justConstructor, body))
            } :+ (Value.Pattern.WildcardPattern(branchOnType), nothingConstructor)
          lambdaType = MorphType.Function((), inputType, maybeOutputType)
        } yield Value.Value.Lambda(
          lambdaType,
          argumentPattern,
          Value.Value.PatternMatch(
            maybeOutputType,
            branchOn,
            transformedCases
          )
        )
      case other =>
        Failure(Exception(s"collect expects a single-case lambda match, got: ${other.getClass}"))
    }

  private def toMapLiteralValue(
    apl: Trees.Apply[?],
    args: List[Trees.Tree[?]],
    inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
  )(using Quotes)(using Contexts.Context): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
    for {
      returnType <- resolveType(apl, inferredGenericTypeArgs).orElse {
                      inferredGenericTypeArgs match {
                        case Some(typeArgs) if typeArgs.size == 2 => Try(StandardTypes.dictReference(typeArgs))
                        case _                                    => Failure(Exception("Could not resolve map type for Map()"))
                      }
                    }
      entryValues <- extractListElements(args)
                       .map(_.map(toMapEntryValue).toTryList)
                       .getOrElse(Failure(Exception("Could not extract map entries from Map()")))
      keyValueTypes <- returnType.extractGenericTypeArgs match {
                         case Some(keyType :: valueType :: Nil) => Success(List(keyType, valueType))
                         case _                                 => Failure(Exception("Map type must have key and value generic arguments"))
                       }
      entryTupleType = MorphType.Tuple((), keyValueTypes)
      entryListType  = StandardTypes.listReference(List(entryTupleType))
      fromListType   = MorphType.Function((), entryListType, returnType)
      fromList: Value.Value[Unit, MorphType.Type[Unit]] = Value.Value.Reference(
                                                            fromListType,
                                                            morphir.ir.FQName.fqn("morphir.SDK")("dict")("fromList")
                                                          )
      entryList = Value.Value.List(
                    entryListType,
                    entryValues
                  )
    } yield Value.Value.Apply(
      returnType,
      fromList,
      entryList
    )

  private def toMapGetValue(
    apl: Trees.Apply[?],
    sel: Trees.Select[?],
    args: List[Trees.Tree[?]],
    inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
  )(using Quotes)(using Contexts.Context): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
    sel match {
      case Trees.Select(qualifier, _) =>
        for {
          returnType  <- resolveType(apl, inferredGenericTypeArgs)
          dictValue   <- expandSubTree(qualifier, inferredGenericTypeArgs = None)
          dictType    <- dictValue.extractType
          keyValue    <- getFunctionArgument(args, dictType.extractGenericTypeArgs.flatMap(_.headOption.map(List(_))))
          keyType     <- keyValue.extractType
          functionType = MorphType.Function((), keyType, MorphType.Function((), dictType, returnType))
          getReference: Value.Value[Unit, MorphType.Type[Unit]] = Value.Value.Reference(
                                                                    functionType,
                                                                    morphir.ir.FQName.fqn("morphir.SDK")("dict")("get")
                                                                  )
          partiallyApplied = Value.Value.Apply(
                               MorphType.Function((), dictType, returnType),
                               getReference,
                               keyValue
                             )
        } yield Value.Value.Apply(
          returnType,
          partiallyApplied,
          dictValue
        )
    }

  private def toMapEntryValue(
    entry: Trees.Tree[?]
  )(using Quotes)(using Contexts.Context): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
    entry match {
      case Trees.Apply(Trees.TypeApply(Trees.Select(arrowAssoc, methodName), _), value :: Nil)
          if methodName.show == "->" =>
        tupleFromArrowAssoc(arrowAssoc, value)
      case Trees.Apply(Trees.Select(arrowAssoc, methodName), value :: Nil) if methodName.show == "->" =>
        tupleFromArrowAssoc(arrowAssoc, value)
      case other =>
        expandSubTree(other, inferredGenericTypeArgs = None).flatMap {
          case tuple @ Value.Value.Tuple(_, elements) if elements.size == 2 => Success(tuple)
          case value                                                        => Failure(Exception(s"Map entry did not lower to a key/value pair: ${value.getClass}"))
        }
    }

  private def tupleFromArrowAssoc(
    arrowAssoc: Trees.Tree[?],
    valueTree: Trees.Tree[?]
  )(using Quotes)(using Contexts.Context): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
    for {
      keyTree    <- arrowAssocKey(arrowAssoc)
      keyValue   <- expandSubTree(keyTree, inferredGenericTypeArgs = None)
      valueValue <- expandSubTree(valueTree, inferredGenericTypeArgs = None)
      keyType    <- keyValue.extractType
      valueType  <- valueValue.extractType
    } yield Value.Value.Tuple(
      MorphType.Tuple((), List(keyType, valueType)),
      List(keyValue, valueValue)
    )

  private def arrowAssocKey(
    arrowAssoc: Trees.Tree[?]
  )(using Quotes)(using Contexts.Context): Try[Trees.Tree[?]] =
    arrowAssoc match {
      case Trees.Apply(_, key :: Nil) =>
        Success(key)
      case Trees.Typed(expr, _) =>
        arrowAssocKey(expr)
      case Trees.Inlined(_, bindings, expansion) if bindings.isEmpty =>
        arrowAssocKey(expansion)
      case other =>
        Failure(Exception(s"Could not extract ArrowAssoc key from ${other.getClass}"))
    }

  private def patternType(
    pattern: Value.Pattern[MorphType.Type[Unit]]
  ): Try[MorphType.Type[Unit]] =
    pattern match {
      case Value.Pattern.AsPattern(t, _, _) => Try(t)
      case Value.Pattern.WildcardPattern(t) => Try(t)
      case other                            => Failure(Exception(s"Unsupported foldLeft lambda pattern: ${other.getClass}"))
    }

  def toValue(apl: Trees.TypeApply[?], inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]])(using
    Quotes
  )(using Contexts.Context): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
    apl match {
      case Trees.TypeApply(Trees.Select(id: Trees.Ident[?], methodName), args) if methodName.show == "apply" =>
        for {
          returnType  <- resolveType(apl, inferredGenericTypeArgs)
          constructor <- StandardFunctions.get(id.symbol, returnType)
        } yield constructor

      case Trees.TypeApply(fun, args) =>
        Failure(
          Exception(s"Apply could not be processed: TypeApply(${fun.getClass},[${args.map(_.getClass).mkString(",")}])")
        )
    }

  def toValue(
    functionId: Trees.Ident[?],
    returnType: MorphType.Type[Unit],
    argsReversed: List[Trees.Tree[?]],
    inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
  )(using Quotes)(using Contexts.Context): Try[Value.Value.Apply[Unit, MorphType.Type[Unit]]] = {
    val maybeGenericTypeArgs = returnType.extractGenericTypeArgs
    argsReversed match {
      case head :: Nil =>
        for {
          argument     <- getFunctionArgument(List(head), maybeGenericTypeArgs)
          argumentType <- argument.extractType
          functionFQN  <- functionId.toFQN
        } yield Value.Value.Apply(
          returnType,
          Value.Value.Reference(
            MorphType.Function(
              (),
              argumentType,
              returnType
            ),
            functionFQN
          ),
          argument
        )

      case head :: tail =>
        for {
          argument     <- getFunctionArgument(List(head), maybeGenericTypeArgs)
          argumentType <- argument.extractType
          nestedApplyReturnType = MorphType.Function(
                                    (),
                                    argumentType,
                                    returnType
                                  )
          nestedApply <- toValue(functionId, nestedApplyReturnType, tail, maybeGenericTypeArgs)
        } yield Value.Value.Apply(
          returnType,
          nestedApply,
          argument
        )

      case x => Failure(Exception(s"Apply arguments is unexpected: $x"))
    }
  }

  private def getFunctionArgument(
    args: List[Trees.Tree[?]],
    inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
  )(using Quotes)(using Contexts.Context): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
    Try(args).map {
      case oneElem :: Nil => oneElem
      case moreElem       => throw Exception(s"Number of args: ${moreElem.size}, but only one is supported")
    }
      .map(oneElem => expandSubTree(oneElem, inferredGenericTypeArgs))
      .flatten

  private def applyArguments(
    function: Value.Value[Unit, MorphType.Type[Unit]],
    args: List[Trees.Tree[?]],
    inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
  )(using Quotes)(using Contexts.Context): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
    args match {
      case Nil => Failure(Exception("Function application requires at least one argument"))
      case head :: tail =>
        for {
          argument <- expandSubTree(head, inferredGenericTypeArgs)
          nextReturnType <- function.extractType.flatMap {
                              case MorphType.Function(_, _, returnType) => Try(returnType)
                              case functionType =>
                                Failure(Exception(s"Cannot apply argument to non-function type: $functionType"))
                            }
          applied = Value.Value.Apply(
                      nextReturnType,
                      function,
                      argument
                    )
          fullyApplied <- if (tail.isEmpty) {
                            Try(applied)
                          } else {
                            applyArguments(
                              applied,
                              tail,
                              nextReturnType.extractGenericTypeArgs.orElse(inferredGenericTypeArgs)
                            )
                          }
        } yield fullyApplied
    }

  private def isListLikeApply(fun: Trees.TypeApply[?])(using Quotes)(using Contexts.Context): Boolean =
    fun match {
      case Trees.TypeApply(Trees.Select(id: Trees.Ident[?], methodName), _) if methodName.show == "apply" =>
        resolveNamespace(id.symbol) match {
          case "List" :: "scala" :: Nil                                => true
          case "List" :: "package" :: "scala" :: Nil                   => true
          case "List" :: "immutable" :: "collection" :: "scala" :: Nil => true
          case "Seq" :: "scala" :: Nil                                 => true
          case "Seq" :: "package" :: "scala" :: Nil                    => true
          case "Seq" :: "collection" :: "scala" :: Nil                 => true
          case "Seq" :: "immutable" :: "collection" :: "scala" :: Nil  => true
          case _                                                       => false
        }
      case _ =>
        false
    }

  private def isMapLiteralApply(
    apl: Trees.Apply[?],
    fun: Trees.TypeApply[?],
    inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
  )(using Quotes)(using Contexts.Context): Boolean =
    isMapApply(fun) || resolveType(apl, inferredGenericTypeArgs).toOption.exists {
      case MorphType.Reference(_, fQName, _) => fQName == morphir.ir.FQName.fqn("morphir.SDK")("dict")("dict")
      case _                                 => false
    }

  private def isMapApply(fun: Trees.TypeApply[?])(using Quotes)(using Contexts.Context): Boolean =
    fun match {
      case Trees.TypeApply(Trees.Select(qualifier, methodName), _) if methodName.show == "apply" =>
        qualifier match {
          case id: Trees.Ident[?]   => isMapNamespace(resolveNamespace(id.symbol))
          case sel: Trees.Select[?] => isMapNamespace(resolveNamespace(sel.symbol))
          case _                    => false
        }
      case _ =>
        false
    }

  private def isMapNamespace(namespace: List[String]): Boolean =
    namespace match {
      case "Map" :: "scala" :: Nil                                => true
      case "Map" :: "package" :: "scala" :: Nil                   => true
      case "Map" :: "collection" :: "scala" :: Nil                => true
      case "Map" :: "immutable" :: "collection" :: "scala" :: Nil => true
      case _                                                      => false
    }

  private def isMapMethodApply(sel: Trees.Select[?], methodName: String)(using
    Quotes
  )(using Contexts.Context): Boolean =
    sel match {
      case Trees.Select(qualifier, selectedMethodName) if selectedMethodName.show == methodName =>
        resolveType(qualifier, inferredGenericTypeArgs = None).toOption.exists {
          case MorphType.Reference(_, fQName, _) => fQName == morphir.ir.FQName.fqn("morphir.SDK")("dict")("dict")
          case _                                 => false
        }
      case _ =>
        false
    }

  private def isListHigherOrderMethodApply(fun: Trees.TypeApply[?], methodName: String)(using
    Quotes
  )(using Contexts.Context): Boolean =
    fun match {
      case Trees.TypeApply(Trees.Select(qualifier, selectedMethodName), _) if selectedMethodName.show == methodName =>
        resolveType(qualifier, inferredGenericTypeArgs = None).toOption.exists {
          case MorphType.Reference(_, fQName, _) => fQName == morphir.ir.FQName.fqn("morphir.SDK")("list")("list")
          case _                                 => false
        }
      case _ =>
        false
    }

  private def isListHigherOrderMethodApply(sel: Trees.Select[?], methodName: String)(using
    Quotes
  )(using Contexts.Context): Boolean =
    sel match {
      case Trees.Select(qualifier, selectedMethodName) if selectedMethodName.show == methodName =>
        resolveType(qualifier, inferredGenericTypeArgs = None).toOption.exists {
          case MorphType.Reference(_, fQName, _) => fQName == morphir.ir.FQName.fqn("morphir.SDK")("list")("list")
          case _                                 => false
        }
      case _ =>
        false
    }

  private def isListFoldLeftApply(fun: Trees.Apply[?])(using Quotes)(using Contexts.Context): Boolean =
    fun match {
      case Trees.Apply(sel: Trees.Select[?], initArgs) =>
        initArgs.size == 1 && isListFoldLeftApply(sel)
      case Trees.Apply(Trees.TypeApply(sel: Trees.Select[?], _), initArgs) =>
        initArgs.size == 1 && isListFoldLeftApply(sel)
      case _ =>
        false
    }

  private def isListFoldLeftApply(sel: Trees.Select[?])(using Quotes)(using Contexts.Context): Boolean =
    isListHigherOrderMethodApply(sel, "foldLeft")

  private def hasListElements(args: List[Trees.Tree[?]]): Boolean =
    extractListElements(args).nonEmpty

  private def extractListElements(args: List[Trees.Tree[?]]): Option[List[Trees.Tree[?]]] =
    args match {
      case Nil           => Some(List.empty)
      case oneArg :: Nil => extractRepeatedArgElements(oneArg)
      case _             => None
    }

  private def extractRepeatedArgElements(arg: Trees.Tree[?]): Option[List[Trees.Tree[?]]] =
    arg match {
      case Trees.SeqLiteral(elements, _)                             => Some(elements)
      case Trees.Typed(expr, _)                                      => extractRepeatedArgElements(expr)
      case Trees.Inlined(_, bindings, expansion) if bindings.isEmpty => extractRepeatedArgElements(expansion)
      case _                                                         => None
    }

  private def isTupleApply(fun: Trees.TypeApply[?], arity: Int)(using Quotes)(using Contexts.Context): Boolean =
    fun match {
      case Trees.TypeApply(Trees.Select(id: Trees.Ident[?], methodName), _) if methodName.show == "apply" =>
        resolveNamespace(id.symbol) match {
          case tupleName :: "scala" :: Nil =>
            tupleName.stripPrefix("Tuple").toIntOption.contains(arity)
          case _ =>
            false
        }
      case _ =>
        false
    }

  private def isEnumConstructorApply(sel: Trees.Select[?])(using Quotes)(using Contexts.Context): Boolean =
    sel match {
      case Trees.Select(qualifier: Trees.Select[?], methodName) if methodName.show == "apply" =>
        isEnumConstructorReference(qualifier)
      case _ =>
        false
    }

  private def isEnumConstructorReference(sel: Trees.Select[?])(using Quotes)(using Contexts.Context): Boolean =
    (sel.symbol.flags.is(Flags.Case) && !sel.symbol.flags.is(Flags.CaseAccessor)) ||
      sel.symbol.companionClass.flags.is(Flags.Case)

  private def toEnumConstructorValue(
    sel: Trees.Select[?],
    args: List[Trees.Tree[?]],
    returnType: MorphType.Type[Unit],
    inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
  )(using Quotes)(using Contexts.Context): Try[Value.Value.Constructor[Unit, MorphType.Type[Unit]]] =
    sel match {
      case Trees.Select(qualifier: Trees.Select[?], _) =>
        for {
          argTypes    <- args.map(expandSubTree(_, inferredGenericTypeArgs).flatMap(_.extractType)).toTryList
          constructor <- toConstructorFQName(qualifier.symbol)
          enumReturnType <-
            qualifier.symbol.owner.typeRef.toType(inferredGenericTypeArgs = None).orElse(Try(returnType))
        } yield Value.Value.Constructor(
          argTypes.foldRight(enumReturnType) { (argType, accType) =>
            MorphType.Function((), argType, accType)
          },
          constructor
        )

      case x =>
        Failure(Exception(s"Enum constructor application could not be resolved from: ${x.getClass}"))
    }
}
