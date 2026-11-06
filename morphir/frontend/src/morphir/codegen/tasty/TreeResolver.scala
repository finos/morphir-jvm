package morphir.codegen.tasty

import dotty.tools.dotc.ast.Trees
import dotty.tools.dotc.core.{ Contexts, Flags, Symbols, Types }
import morphir.codegen.tasty.MorphUtils.*
import morphir.ir.{ FQName, Name, Value, Type as MorphType }
import morphir.sdk.List as MorphList

import scala.quoted.Quotes
import scala.util.{ Failure, Success, Try }

trait TreeResolver {
  private val scalaListTypeNamespaces = Set(
    List("List", "scala"),
    List("List", "package", "scala"),
    List("List", "immutable", "collection", "scala")
  )

  private val scalaSeqTypeNamespaces = Set(
    List("Seq", "scala"),
    List("Seq", "package", "scala"),
    List("Seq", "collection", "scala"),
    List("Seq", "immutable", "collection", "scala")
  )

  private val scalaMapTypeNamespaces = Set(
    List("Map", "scala"),
    List("Map", "package", "scala"),
    List("Map", "collection", "scala"),
    List("Map", "immutable", "collection", "scala")
  )

  private val unitValueNamespaces = Set(
    List("()", "scala"),
    List("()", "Predef", "scala"),
    List("()", "package", "scala"),
    List("UNIT", "BoxedUnit", "runtime", "scala")
  )

  def resolveType(tree: Trees.Tree[?], inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]])(using
    Quotes
  )(using Contexts.Context): Try[MorphType.Type[Unit]] =
    resolveTypeOpt(tree.typeOpt, inferredGenericTypeArgs)

  private def resolveTypeOpt(
    typeOpt: Types.Type,
    inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
  )(using Quotes)(using Contexts.Context): Try[MorphType.Type[Unit]] =
    Try(typeOpt)
      .filter(tpe => tpe.typeSymbol.isType && !tpe.isInstanceOf[Types.MethodType])
      .flatMap(_.toType(inferredGenericTypeArgs))
      .orElse {
        typeOpt match {
          case Types.TermRef(_, sbl: Symbols.Symbol) => sbl.denot.info.resultType.toType(inferredGenericTypeArgs)
          case ctr: Types.CachedTypeRef              => ctr.symbol.denot.info.resultType.toType(inferredGenericTypeArgs)
          case cat: Types.AppliedType                => cat.toType(inferredGenericTypeArgs)
          case met: Types.MethodType                 => resolveMethodType(met, inferredGenericTypeArgs)
          case ot: Types.OrType if ot.baseClasses.nonEmpty =>
            ot.baseClasses.head.localReturnType.toType(inferredGenericTypeArgs)
          case typeOpt => typeOpt.toType(inferredGenericTypeArgs)
          // case x => Failure(Exception(s"Type could not be resolved: ${x.getClass}"))
        }
      }

  private def resolveMethodType(
    met: Types.MethodType,
    inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
  )(using Quotes)(using Contexts.Context): Try[MorphType.Type[Unit]] = {
    val paramTypes: List[Try[MorphType.Type[Unit]]] =
      met.paramInfos.map(p => resolveTypeOpt(p, inferredGenericTypeArgs))

    for {
      params     <- paramTypes.toTryList
      returnType <- resolveTypeOpt(met.resType, inferredGenericTypeArgs)
    } yield params.foldRight(returnType) { (paramType, accType) =>
      MorphType.Function((), paramType, accType)
    }
  }

  // In some cases the Scala AST does not contain generic type information while the Morphir AST still needs one.
  // Here is an example:
  //   def maybePositive(a: Int): Option[Int] = if (a > 0) Some(a) else None
  // Neither the 'If' node nor the 'None' node know about the generic type 'Int'.
  // For now I will go with the assumption that a node higher up in the AST hierarchy will know the generic type.
  // In the above example the method return type has the generic type information, which is then passed down the tree.
  // The 'If' node has an 'Option' return type, which inherits the generic type from the method's return type.
  // The same is true for the 'None' node, which inherits the generic type from the 'If'.
  // However 'Some(a)' does not need the inherited generic type as that information is available within the 'Some' node.
  extension (typeOpt: Types.Type)(using Quotes)(using Contexts.Context)
    def toType(inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]): Try[MorphType.Type[Unit]] = {
      val maybeTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]] =
        Option(typeOpt).collect { case Types.AppliedType(_, args) => args }.flatMap { args =>
          val resolvedArgs = args.map(_.toType(inferredGenericTypeArgs = None)).collect { case Success(t) => t }
          Option.when(resolvedArgs.size == args.size)(resolvedArgs)
        }
          .orElse(inferredGenericTypeArgs)

      Try {
        if typeOpt.typeSymbol.flags.is(Flags.Param) then
          resolveTypeParameter(typeOpt.typeSymbol, inferredGenericTypeArgs)
        else {
          val symbolNamespace = resolveNamespace(typeOpt.typeSymbol)
          (symbolNamespace, maybeTypeArgs) match {
            case ("Boolean" :: "scala" :: Nil, _) =>
              StandardTypes.boolReference
            case ("Byte" :: "scala" :: Nil, _) =>
              StandardTypes.intReference
            case ("Short" :: "scala" :: Nil, _) =>
              StandardTypes.intReference
            case ("Int" :: "scala" :: Nil, _) =>
              StandardTypes.intReference
            case ("Long" :: "scala" :: Nil, _) =>
              StandardTypes.intReference
            case ("Float" :: "scala" :: Nil, _) =>
              StandardTypes.floatReference
            case ("Double" :: "scala" :: Nil, _) =>
              StandardTypes.floatReference
            case ("Char" :: "scala" :: Nil, _) =>
              StandardTypes.charReference
            case ("String" :: "Predef" :: "scala" :: Nil, _) =>
              StandardTypes.stringReference
            case ("String" :: "lang" :: "java" :: Nil, _) =>
              StandardTypes.stringReference
            case ("BigDecimal" :: "math" :: "scala" :: Nil, _) =>
              StandardTypes.decimalReference
            case ("BigDecimal" :: "package" :: "scala" :: Nil, _) => // type alias to scala.math.BigDecimal
              StandardTypes.decimalReference
            case ("Unit" :: "scala" :: Nil, _) =>
              StandardTypes.unitType
            case (tupleName :: "scala" :: Nil, Some(typeArgs)) if isTupleTypeName(tupleName, typeArgs.size) =>
              MorphType.Tuple((), typeArgs)
            case (namespace, Some(typeArgs))
                if matchesParameterizedReference(namespace, scalaListTypeNamespaces, typeArgs, 1) =>
              StandardTypes.listReference(typeArgs)
            case (namespace, Some(typeArgs))
                if matchesParameterizedReference(namespace, scalaSeqTypeNamespaces, typeArgs, 1) =>
              StandardTypes.listReference(typeArgs)
            case (namespace, Some(typeArgs))
                if matchesParameterizedReference(namespace, scalaMapTypeNamespaces, typeArgs, 2) =>
              StandardTypes.dictReference(typeArgs)
            case ("Option" :: "scala" :: Nil, Some(typeArgs)) if typeArgs.size == 1 =>
              StandardTypes.maybeReference(typeArgs)
            case ("Some" :: "scala" :: Nil, Some(typeArgs)) if typeArgs.size == 1 =>
              StandardTypes.maybeReference(typeArgs)
            case ("None" :: "scala" :: Nil, Some(typeArgs)) if typeArgs.size == 1 =>
              StandardTypes.maybeReference(typeArgs)
            case (typeName :: packageParts, typeArgs) if packageParts.nonEmpty && !packageParts.contains("scala") =>
              val fQName = FQName.fqn(packageParts.reverse.mkString("."))(typeName)(typeName)
              MorphType.Reference((), fQName, typeArgs.getOrElse(MorphList.empty))
            case (name, typeArgs) =>
              throw UnsupportedOperationException(s"Type name: $name is not supported with type args: $typeArgs")
          }
        }
      }
    }

  extension (morphirType: MorphType.Type[Unit])
    def extractGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]] =
      Some(morphirType).collect { case MorphType.Reference(_, _, types) if types.nonEmpty => types }

  private def isTupleTypeName(typeName: String, arity: Int): Boolean =
    typeName
      .stripPrefix("Tuple")
      .toIntOption
      .contains(arity)

  private def matchesParameterizedReference(
    symbolNamespace: List[String],
    expectedNamespaces: Set[List[String]],
    typeArgs: MorphList.List[MorphType.Type[Unit]],
    expectedArity: Int
  ): Boolean =
    expectedNamespaces.contains(symbolNamespace) && typeArgs.size == expectedArity

  protected def isUnitValueSymbol(symbolNamespace: List[String]): Boolean =
    unitValueNamespaces.contains(symbolNamespace)

  protected def toUnitValue: Value.Value.Unit[Unit, MorphType.Type[Unit]] =
    Value.Value.Unit(StandardTypes.unitType)

  private def resolveTypeParameter(
    typeParamSymbol: Symbols.Symbol,
    inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]]
  )(using Quotes)(using Contexts.Context): MorphType.Type[Unit] =
    inferredGenericTypeArgs.flatMap { typeArgs =>
      val ownerTypeParams = typeParamSymbol.owner.typeParams
      val paramIndex      = ownerTypeParams.indexWhere(_.name.show == typeParamSymbol.name.show)
      typeArgs.lift(paramIndex)
    }.orElse {
      inferredGenericTypeArgs.collect { case typeArg :: Nil =>
        typeArg
      }
    }
      .getOrElse(MorphType.Variable((), Name.fromString(typeParamSymbol.name.show)))

  def expandSubTree(tree: Trees.Tree[?], inferredGenericTypeArgs: Option[MorphList.List[MorphType.Type[Unit]]])(using
    Quotes
  )(using Contexts.Context): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
    tree match {
      case apl: Trees.Apply[?]   => apl.toValue(inferredGenericTypeArgs)
      case block: Trees.Block[?] => BlockMorph.toValue(block, inferredGenericTypeArgs)
      case ident: Trees.Ident[?] => ident.toValue(inferredGenericTypeArgs)
      case Trees.Inlined(_, bindings, expansion) =>
        if (bindings.isEmpty) expandSubTree(expansion, inferredGenericTypeArgs)
        else BlockMorph.toValue(bindings, expansion, inferredGenericTypeArgs)
      case lit: Trees.Literal[?] => lit.toValue
      case mtch: Trees.Match[?]  => mtch.toValue(inferredGenericTypeArgs)
      case sel: Trees.Select[?]  => sel.toValue(inferredGenericTypeArgs)
      case Trees.Typed(expr, _)  => expandSubTree(expr, inferredGenericTypeArgs)
      case ifs: Trees.If[?]      => ifs.toValue(inferredGenericTypeArgs)
      case x                     => Failure(Exception(s"Type not supported: ${x.getClass}"))
    }

  protected def toConstructorFQName(symbol: Symbols.Symbol)(using Quotes)(using Contexts.Context): Try[FQName.FQName] =
    resolveNamespace(symbol) match {
      case localName :: moduleName :: packageName if packageName.nonEmpty && !packageName.contains("scala") =>
        Success(FQName.fqn(packageName.reverse.mkString("."))(moduleName)(localName))
      case x =>
        Failure(Exception(s"Could not resolve constructor FQName from: $x"))
    }
}
