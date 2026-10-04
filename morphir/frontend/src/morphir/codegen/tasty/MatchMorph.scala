package morphir.codegen.tasty

import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.core.{ Contexts, Flags, Symbols }
import morphir.codegen.tasty.MorphUtils.*
import morphir.ir.{ FQName, Name, Value, Type as MorphType }
import morphir.sdk.List as MorphList

import scala.quoted.Quotes
import scala.util.{ Failure, Success, Try }

object MatchMorph extends TreeResolver {

  def toValue(
    mtch: Match[?],
    inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
  )(using Quotes)(using Contexts.Context): Try[Value.Value.PatternMatch[Unit, MorphType.Type[Unit]]] =
    for {
      returnType          <- resolveType(mtch, inferredGenericTypeArgs)
      branchOn            <- expandSubTree(mtch.selector, inferredGenericTypeArgs)
      branchOnType        <- branchOn.extractType
      maybeGenericTypeArgs = returnType.extractGenericTypeArgs
      cases               <- mtch.cases.map(toCase(_, branchOnType, maybeGenericTypeArgs)).toTryList
    } yield Value.Value.PatternMatch(
      returnType,
      branchOn,
      cases
    )

  private def toCase(
    caseDef: CaseDef[?],
    branchOnType: MorphType.Type[Unit],
    inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
  )(using
    Quotes
  )(using Contexts.Context): Try[(Value.Pattern[MorphType.Type[Unit]], Value.Value[Unit, MorphType.Type[Unit]])] =
    for {
      _ <- if caseDef.guard.isEmpty then Success(())
           else Failure(NotImplementedError(s"Match guards are not supported: ${caseDef.guard.getClass}"))
      pattern <- toPattern(caseDef.pat, branchOnType)
      body    <- expandSubTree(caseDef.body, inferredGenericTypeArgs)
    } yield (pattern, body)

  private def toPattern(
    patternTree: Tree[?],
    expectedType: MorphType.Type[Unit]
  )(using Quotes)(using Contexts.Context): Try[Value.Pattern[MorphType.Type[Unit]]] =
    patternTree match {
      case Bind(name, body) =>
        for {
          bindType     <- resolveType(patternTree, inferredGenericTypeArgs = None)
          innerPattern <- toPattern(body, bindType)
        } yield Value.Pattern.AsPattern(
          bindType,
          innerPattern,
          Name.fromString(name.show)
        )

      case Ident(name) if name.show == "_" =>
        Success(Value.Pattern.WildcardPattern(expectedType))

      case Typed(expr, _) =>
        toPattern(expr, expectedType)

      case lit: Literal[?] =>
        LiteralMorph.toPattern(lit)

      case UnApply(fun, _, patterns) =>
        tupleElementTypes(expectedType) match {
          case Some(elementTypes) if isTupleUnapply(fun, patterns.size) =>
            toTuplePattern(patterns, expectedType, elementTypes)

          case _ =>
            toConstructorPattern(fun, patterns, expectedType)
        }

      case Ident(name) if name.show == "None" =>
        Success(
          Value.Pattern.ConstructorPattern(
            expectedType,
            maybeConstructor("nothing"),
            List.empty
          )
        )

      case sel: Select[?] if isEnumConstructor(sel) =>
        toConstructorFQName(sel.symbol).map { constructorFQName =>
          Value.Pattern.ConstructorPattern(
            expectedType,
            constructorFQName,
            List.empty
          )
        }

      case x =>
        Failure(NotImplementedError(s"Pattern is not supported: ${x.getClass}"))
    }

  private def toTuplePattern(
    patterns: List[Tree[?]],
    expectedType: MorphType.Type[Unit],
    elementTypes: List[MorphType.Type[Unit]]
  )(using Quotes)(using Contexts.Context): Try[Value.Pattern.TuplePattern[MorphType.Type[Unit]]] =
    if patterns.size != elementTypes.size then
      Failure(NotImplementedError(s"Tuple pattern arity mismatch: expected ${elementTypes.size}, got ${patterns.size}"))
    else
      patterns
        .zip(elementTypes)
        .map { case (pattern, elementType) =>
          toPattern(pattern, elementType)
        }
        .toTryList
        .map { elements =>
          Value.Pattern.TuplePattern(expectedType, elements)
        }

  private def toConstructorArgs(
    patterns: List[Tree[?]],
    expectedType: MorphType.Type[Unit],
    constructorFQName: FQName.FQName,
    constructorArgTypes: Option[List[MorphType.Type[Unit]]] = None
  )(using Quotes)(using Contexts.Context): Try[List[Value.Pattern[MorphType.Type[Unit]]]] =
    constructorFQName match {
      case (_, _, localName) if localName == Name.fromString("just") =>
        expectedType.extractGenericTypeArgs match {
          case Some(innerType :: Nil) if patterns.size == 1 =>
            patterns.map(toPattern(_, innerType)).toTryList
          case Some(_) =>
            Failure(NotImplementedError(s"Option Some pattern must have exactly one argument: ${patterns.size}"))
          case None =>
            Failure(NotImplementedError(s"Could not resolve Some pattern type from: $expectedType"))
        }

      case (_, _, localName) if localName == Name.fromString("nothing") && patterns.isEmpty =>
        Success(List.empty)

      case (_, _, _) =>
        constructorArgTypes match {
          case Some(argTypes) if argTypes.size == patterns.size =>
            patterns
              .zip(argTypes)
              .map { case (pattern, argType) =>
                toPattern(pattern, argType)
              }
              .toTryList
          case Some(argTypes) =>
            Failure(
              NotImplementedError(
                s"Constructor pattern arity mismatch: expected ${argTypes.size}, got ${patterns.size}"
              )
            )
          case None =>
            Failure(NotImplementedError(s"Constructor pattern is not supported: $constructorFQName"))
        }
    }

  private def toOptionConstructorFQName(fun: Tree[?])(using Quotes)(using Contexts.Context): Try[FQName.FQName] =
    unwrapTypeApply(fun) match {
      case Select(qualifier, name) if qualifier.symbol.name.show == "Some" && name.show == "unapply" =>
        Success(maybeConstructor("just"))
      case Select(qualifier, name) if qualifier.symbol.name.show == "None" && name.show == "unapply" =>
        Success(maybeConstructor("nothing"))
      case Select(_, name) if name.show == "unapplySeq" =>
        Failure(NotImplementedError("Sequence extractor patterns are not supported"))
      case x =>
        Failure(NotImplementedError(s"Pattern extractor is not supported: ${x.getClass}"))
    }

  private def isTupleUnapply(fun: Tree[?], arity: Int)(using Quotes)(using Contexts.Context): Boolean =
    unwrapTypeApply(fun) match {
      case Select(qualifier, name) =>
        qualifier.symbol.name.show == s"Tuple$arity" && name.show == "unapply"
      case _ =>
        false
    }

  private def tupleElementTypes(expectedType: MorphType.Type[Unit]): Option[List[MorphType.Type[Unit]]] =
    expectedType match {
      case MorphType.Tuple(_, elementTypes) => Some(elementTypes)
      case _                                => None
    }

  private def toConstructorPattern(
    fun: Tree[?],
    patterns: List[Tree[?]],
    expectedType: MorphType.Type[Unit]
  )(using Quotes)(using Contexts.Context): Try[Value.Pattern.ConstructorPattern[MorphType.Type[Unit]]] =
    for {
      (constructorFQName, constructorArgTypes) <- constructorPatternInfo(fun)
      constructorArgs                          <- toConstructorArgs(patterns, expectedType, constructorFQName, constructorArgTypes)
    } yield Value.Pattern.ConstructorPattern(
      expectedType,
      constructorFQName,
      constructorArgs
    )

  private def constructorPatternInfo(
    fun: Tree[?]
  )(using Quotes)(using Contexts.Context): Try[(FQName.FQName, Option[List[MorphType.Type[Unit]]])] =
    unwrapTypeApply(fun) match {
      case Select(qualifier, name) if qualifier.symbol.name.show == "Some" && name.show == "unapply" =>
        Success((maybeConstructor("just"), None))

      case Select(qualifier, name) if qualifier.symbol.name.show == "None" && name.show == "unapply" =>
        Success((maybeConstructor("nothing"), Some(List.empty)))

      case Select(qualifier: Select[?], name) if name.show == "unapply" && isEnumConstructor(qualifier) =>
        for {
          constructorFQName   <- toConstructorFQName(qualifier.symbol)
          constructorArgTypes <- enumConstructorArgTypes(qualifier.symbol)
        } yield (constructorFQName, Some(constructorArgTypes))

      case Select(_, name) if name.show == "unapplySeq" =>
        Failure(NotImplementedError("Sequence extractor patterns are not supported"))

      case x =>
        Failure(NotImplementedError(s"Pattern extractor is not supported: ${x.getClass}"))
    }

  private def enumConstructorArgTypes(
    constructorSymbol: Symbols.Symbol
  )(using Quotes)(using Contexts.Context): Try[List[MorphType.Type[Unit]]] =
    constructorSymbol.companionClass.caseAccessors.map { accessor =>
      accessor.denot.info.resultType.toType(inferredGenericTypeArgs = None)
    }.toTryList

  private def unwrapTypeApply(tree: Tree[?]): Tree[?] =
    tree match {
      case TypeApply(fun, _) => unwrapTypeApply(fun)
      case other             => other
    }

  private def maybeConstructor(localName: String): FQName.FQName =
    FQName.fqn("morphir.SDK")("maybe")(localName)

  private def isEnumConstructor(sel: Select[?])(using Quotes)(using Contexts.Context): Boolean =
    (sel.symbol.flags.is(Flags.Case) && !sel.symbol.flags.is(Flags.CaseAccessor)) ||
      sel.symbol.companionClass.flags.is(Flags.Case)
}
