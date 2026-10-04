package morphir.codegen.tasty

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.core.{ Contexts, Symbols }
import morphir.codegen.tasty.MorphUtils.*
import morphir.ir.{ Name, Value, Type as MorphType }
import morphir.sdk.List as MorphList
import scala.collection.immutable.ListMap

import scala.quoted.Quotes
import scala.util.{ Failure, Try }

object BlockMorph extends TreeResolver {

  def toValue(block: Trees.Block[?], inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]])(using
    Quotes
  )(using Contexts.Context): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
    block match {
      case lambdaBlock if isLambdaBlock(lambdaBlock) =>
        toLambdaValue(lambdaBlock, inferredGenericTypeArgs)
      case Trees.Block(Nil, expr) =>
        expandSubTree(expr, inferredGenericTypeArgs)

      case Trees.Block(stats, expr) =>
        toValue(stats, expr, inferredGenericTypeArgs)
    }

  def toValue(
    stats: List[Trees.Tree[?]],
    expr: Trees.Tree[?],
    inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
  )(using Quotes)(using Contexts.Context): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
    for {
      blockType  <- resolveType(expr, inferredGenericTypeArgs)
      blockValue <- toValue(stats, expr, blockType, inferredGenericTypeArgs)
    } yield collapseTupleDestructuring(blockValue)

  private def toValue(
    stats: List[Trees.Tree[?]],
    expr: Trees.Tree[?],
    blockType: MorphType.Type[Unit],
    inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
  )(using Quotes)(using Contexts.Context): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
    stats match {
      case Nil =>
        expandSubTree(expr, blockType.extractGenericTypeArgs.orElse(inferredGenericTypeArgs))

      case _ =>
        tupleDestructuringStat(stats, blockType, inferredGenericTypeArgs) match {
          case Some(destructuring) =>
            for {
              valueToDestructure <- expandSubTree(destructuring.selector, inferredGenericTypeArgs)
              inValue            <- toValue(destructuring.remainingStats, expr, blockType, inferredGenericTypeArgs)
            } yield Value.Value.Destructure(
              blockType,
              destructuring.pattern,
              valueToDestructure,
              inValue
            )

          case None =>
            for {
              inValue  <- toValue(stats.tail, expr, blockType, inferredGenericTypeArgs)
              letValue <- statToLetDefinition(stats.head, inValue, blockType, inferredGenericTypeArgs)
            } yield letValue
        }
    }

  private def statToLetDefinition(
    stat: Trees.Tree[?],
    inValue: Value.Value[Unit, MorphType.Type[Unit]],
    blockType: MorphType.Type[Unit],
    inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
  )(using Quotes)(using Contexts.Context): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
    stat match {
      case valDef: Trees.ValDef[?] if !valDef.rhs.isEmpty =>
        for {
          definitionType <- resolveType(stat, inferredGenericTypeArgs)
          definitionBody <-
            expandSubTree(valDef.rhs, definitionType.extractGenericTypeArgs.orElse(inferredGenericTypeArgs))
        } yield Value.Value.LetDefinition(
          blockType,
          Name.fromString(valDef.name.show),
          Value.Definition(
            inputTypes = MorphList.empty,
            outputType = definitionType,
            body = definitionBody
          ),
          inValue
        )

      case valDef: Trees.ValDef[?] =>
        Failure(Exception(s"Local val without rhs is not supported: ${valDef.name.show}"))

      case x =>
        Failure(Exception(s"Block statement is not supported: ${x.getClass}"))
    }

  private def toLambdaValue(
    block: Trees.Block[?],
    inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
  )(using Quotes)(using Contexts.Context): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
    block match {
      case Trees.Block((dd: Trees.DefDef[?]) :: Nil, Trees.Closure(_, meth, _))
          if meth.symbol == dd.symbol && dd.termParamss.size == 1 && dd.termParamss.head.nonEmpty =>
        val params = dd.termParamss.head
        for {
          paramTypes <- params.map(resolveType(_, inferredGenericTypeArgs = None)).toTryList
          bodyType   <- resolveType(dd.rhs, inferredGenericTypeArgs = None)
          body       <- expandSubTree(dd.rhs, bodyType.extractGenericTypeArgs.orElse(inferredGenericTypeArgs))
          lambda     <- buildLambdaValue(params.zip(paramTypes), body)
        } yield lambda

      case x =>
        Failure(Exception(s"Lambda block is not supported: ${x.getClass}"))
    }

  private def buildLambdaValue(
    paramsWithTypes: List[(Trees.ValDef[?], MorphType.Type[Unit])],
    body: Value.Value[Unit, MorphType.Type[Unit]]
  ): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
    paramsWithTypes.reverse.foldLeft(Try(body: Value.Value[Unit, MorphType.Type[Unit]])) {
      case (innerTry, (param, paramType)) =>
        for {
          innerBody <- innerTry
          innerType <- innerBody.extractType
        } yield Value.Value.Lambda(
          MorphType.Function((), paramType, innerType),
          Value.Pattern.AsPattern(
            paramType,
            Value.Pattern.WildcardPattern(paramType),
            Name.fromString(param.name.toString)
          ),
          innerBody
        )
    }

  private def isLambdaBlock(block: Trees.Block[?])(using Quotes)(using Contexts.Context): Boolean =
    block match {
      case Trees.Block((dd: Trees.DefDef[?]) :: Nil, Trees.Closure(_, meth, _)) =>
        meth.symbol == dd.symbol && dd.termParamss.size == 1 && dd.termParamss.head.nonEmpty
      case _ =>
        false
    }

  private case class TupleDestructuringStat(
    pattern: Value.Pattern.TuplePattern[MorphType.Type[Unit]],
    selector: Trees.Tree[?],
    remainingStats: List[Trees.Tree[?]]
  )

  private def tupleDestructuringStat(
    stats: List[Trees.Tree[?]],
    blockType: MorphType.Type[Unit],
    inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
  )(using Quotes)(using Contexts.Context): Option[TupleDestructuringStat] =
    stats match {
      case (tempVal: Trees.ValDef[?]) :: tail if !tempVal.rhs.isEmpty =>
        tupleDestructuringPattern(tempVal, inferredGenericTypeArgs).flatMap {
          case (selector, tupleType, elementTypes) =>
            val projectionBindings = tupleProjectionBindings(tempVal.symbol, tail)
            val usedProjections    = projectionBindings.size

            if usedProjections == 0 then None
            else if usedProjections > elementTypes.size then None
            else {
              val patternElements: List[Value.Pattern[MorphType.Type[Unit]]] = elementTypes.zipWithIndex.map {
                case (elementType, index) =>
                  projectionBindings.get(index + 1) match {
                    case Some(boundName) =>
                      Value.Pattern.AsPattern(
                        elementType,
                        Value.Pattern.WildcardPattern(elementType),
                        Name.fromString(boundName)
                      )
                    case None =>
                      Value.Pattern.WildcardPattern(elementType)
                  }
              }

              Some(
                TupleDestructuringStat(
                  Value.Pattern.TuplePattern(
                    tupleType,
                    patternElements
                  ),
                  selector,
                  tail.drop(usedProjections)
                )
              )
            }
        }

      case _ =>
        None
    }

  private def tupleDestructuringPattern(
    tempVal: Trees.ValDef[?],
    inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
  )(using Quotes)(using Contexts.Context): Option[(Trees.Tree[?], MorphType.Type[Unit], List[MorphType.Type[Unit]])] =
    tempVal.rhs match {
      case Trees.Match(selector, caseDef :: Nil) =>
        for {
          tupleType <- resolveType(tempVal, inferredGenericTypeArgs).toOption.collect {
                         case tupleType @ MorphType.Tuple(_, elementTypes) => (tupleType, elementTypes)
                       }
          (_, elementTypes) = tupleType
          _                <- tupleCaseArity(caseDef).filter(_ == elementTypes.size)
        } yield (selector, tupleType._1, elementTypes)

      case _ =>
        None
    }

  private def tupleCaseArity(caseDef: Trees.CaseDef[?])(using Quotes)(using Contexts.Context): Option[Int] =
    tupleCaseArity(caseDef.pat)

  private def tupleCaseArity(pattern: Trees.Tree[?])(using Quotes)(using Contexts.Context): Option[Int] =
    pattern match {
      case Trees.Bind(_, body) =>
        tupleCaseArity(body)
      case Trees.Typed(expr, _) =>
        tupleCaseArity(expr)
      case Trees.UnApply(fun, _, patterns) if isTupleUnapply(fun, patterns.size) =>
        Some(patterns.size)
      case _ =>
        None
    }

  private def tupleProjectionBindings(
    tupleTempSymbol: Symbols.Symbol,
    stats: List[Trees.Tree[?]]
  )(using Quotes)(using Contexts.Context): ListMap[Int, String] =
    stats.takeWhile {
      case Trees.ValDef(_, rhs, _) => tupleProjectionBinding(tupleTempSymbol, rhs).isDefined
      case _                       => false
    }.flatMap {
      case Trees.ValDef(bindingName, rhs, _) =>
        tupleProjectionBinding(tupleTempSymbol, rhs).map(_ -> bindingName.show)
      case _ =>
        None
    }
      .foldLeft(ListMap.empty[Int, String]) { case (acc, (index, bindingName)) =>
        acc.updated(index, bindingName)
      }

  private def tupleProjectionBinding(tupleTempSymbol: Symbols.Symbol, rhs: Trees.Tree[?])(using
    Quotes
  )(using Contexts.Context): Option[Int] =
    rhs match {
      case Trees.Select(qualifier: Trees.Ident[?], fieldName) if qualifier.symbol == tupleTempSymbol =>
        fieldName.show.stripPrefix("_").toIntOption
      case Trees.Typed(expr, _) =>
        tupleProjectionBinding(tupleTempSymbol, expr)
      case _ =>
        None
    }

  private def isTupleUnapply(fun: Trees.Tree[?], arity: Int)(using Quotes)(using Contexts.Context): Boolean =
    unwrapTypeApply(fun) match {
      case Trees.Select(qualifier, name) =>
        qualifier.symbol.name.show == s"Tuple$arity" && name.show == "unapply"
      case _ =>
        false
    }

  private def unwrapTypeApply(tree: Trees.Tree[?]): Trees.Tree[?] =
    tree match {
      case Trees.TypeApply(fun, _) => unwrapTypeApply(fun)
      case other                   => other
    }

  private def collapseTupleDestructuring(
    value: Value.Value[Unit, MorphType.Type[Unit]]
  ): Value.Value[Unit, MorphType.Type[Unit]] =
    value match {
      case Value.Value.LetDefinition(attributes, tempName, definition, inValue) =>
        collapseTupleDestructuring(definition.body, tempName, inValue) match {
          case Some((pattern, selector, rest)) =>
            Value.Value.Destructure(attributes, pattern, selector, rest)
          case None =>
            value
        }

      case _ =>
        value
    }

  private def collapseTupleDestructuring(
    tupleMatchBody: Value.Value[Unit, MorphType.Type[Unit]],
    tempName: Name.Name,
    inValue: Value.Value[Unit, MorphType.Type[Unit]]
  ): Option[
    (
      Value.Pattern.TuplePattern[MorphType.Type[Unit]],
      Value.Value[Unit, MorphType.Type[Unit]],
      Value.Value[Unit, MorphType.Type[Unit]]
    )
  ] =
    tupleMatchBody match {
      case Value.Value.PatternMatch(
            _,
            selector,
            (tuplePattern: Value.Pattern.TuplePattern[MorphType.Type[Unit]], _) :: Nil
          ) =>
        stripTupleProjectionLets(inValue, tempName, tuplePattern).map(rest => (tuplePattern, selector, rest))

      case _ =>
        None
    }

  private def stripTupleProjectionLets(
    value: Value.Value[Unit, MorphType.Type[Unit]],
    tempName: Name.Name,
    tuplePattern: Value.Pattern.TuplePattern[MorphType.Type[Unit]]
  ): Option[Value.Value[Unit, MorphType.Type[Unit]]] = {
    val boundFields =
      tuplePattern match {
        case Value.Pattern.TuplePattern(_, elements) =>
          elements.zipWithIndex.collect {
            case (Value.Pattern.AsPattern(_, Value.Pattern.WildcardPattern(_), boundName), index) =>
              (boundName, Name.fromString((index + 1).toString))
          }
      }

    boundFields.foldLeft(Option(value)) { case (currentValue, (boundName, fieldName)) =>
      currentValue.flatMap {
        case Value.Value.LetDefinition(_, `boundName`, definition, next) if definition.inputTypes.isEmpty =>
          definition.body match {
            case Value.Value.Field(_, Value.Value.Variable(_, variableName), `fieldName`) if variableName == tempName =>
              Some(next)
            case _ =>
              None
          }

        case _ =>
          None
      }
    }
  }
}
