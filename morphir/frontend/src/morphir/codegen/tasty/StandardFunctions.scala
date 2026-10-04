package morphir.codegen.tasty

import dotty.tools.dotc.core.{ Contexts, Symbols }
import morphir.codegen.tasty.MorphUtils.*
import morphir.ir.{ Type as MorphType, * }

import scala.quoted.Quotes
import scala.util.{ Failure, Success, Try }

object StandardFunctions {
  private val basicsPackage = FQName.fqn("morphir.SDK")("basics")

  private def isDecimalType(morphType: MorphType.Type[Unit]): Boolean =
    morphType == StandardTypes.decimalReference

  private def isListType(morphType: MorphType.Type[Unit]): Boolean =
    morphType match {
      case MorphType.Reference(_, fQName, _) => fQName == FQName.fqn("morphir.SDK")("list")("list")
      case _                                 => false
    }

  def get(symbol: Symbols.Symbol, returnType: MorphType.Type[Unit], argumentType: MorphType.Type[Unit])(using Quotes)(
    using Contexts.Context
  ): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
    get(symbol, returnType, Some(argumentType))

  def get(symbol: Symbols.Symbol, returnType: MorphType.Type[Unit])(using Quotes)(using
    Contexts.Context
  ): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
    get(symbol, returnType, None)

  def getCollectionMethod(
    symbol: Symbols.Symbol,
    returnType: MorphType.Type[Unit],
    argumentTypes: List[MorphType.Type[Unit]]
  )(using Quotes)(using Contexts.Context): Try[Value.Value.Reference[Unit, MorphType.Type[Unit]]] = {
    val symbolNamespace = resolveNamespace(symbol)
    (symbolNamespace, argumentTypes) match {
      case ("map" :: _, functionType :: listType :: Nil) if isListType(listType) =>
        Success(
          toCurriedFunctionReference(FQName.fqn("morphir.SDK")("list")("map"), returnType, List(functionType, listType))
        )
      case ("filter" :: _, functionType :: listType :: Nil) if isListType(listType) =>
        Success(
          toCurriedFunctionReference(
            FQName.fqn("morphir.SDK")("list")("filter"),
            returnType,
            List(functionType, listType)
          )
        )
      case ("flatMap" :: _, functionType :: listType :: Nil) if isListType(listType) =>
        Success(
          toCurriedFunctionReference(
            FQName.fqn("morphir.SDK")("list")("concatMap"),
            returnType,
            List(functionType, listType)
          )
        )
      case ("collect" :: _, functionType :: listType :: Nil) if isListType(listType) =>
        Success(
          toCurriedFunctionReference(
            FQName.fqn("morphir.SDK")("list")("filterMap"),
            returnType,
            List(functionType, listType)
          )
        )
      case ("foldLeft" :: _, functionType :: initType :: listType :: Nil) if isListType(listType) =>
        Success(
          toCurriedFunctionReference(
            FQName.fqn("morphir.SDK")("list")("foldl"),
            returnType,
            List(functionType, initType, listType)
          )
        )
      case x =>
        Failure(
          Exception(
            s"Collection method for symbol ${symbolNamespace.mkString(",")} not found with args ${argumentTypes.mkString(",")}"
          )
        )
    }
  }

  private def get(
    symbol: Symbols.Symbol,
    returnType: MorphType.Type[Unit],
    maybeArgumentType: Option[MorphType.Type[Unit]]
  )(using Quotes)(using Contexts.Context): Try[Value.Value[Unit, MorphType.Type[Unit]]] = {
    val symbolNamespace = resolveNamespace(symbol)
    maybeArgumentType match {
      case Some(argumentType) =>
        val operator = symbol.name.show

        if (isDecimalType(argumentType)) {
          operator match {
            case "+" =>
              Success(toFunctionReference(FQName.fqn("morphir.SDK")("decimal")("add"), returnType, argumentType))
            case "-" =>
              Success(toFunctionReference(FQName.fqn("morphir.SDK")("decimal")("sub"), returnType, argumentType))
            case "*" =>
              Success(toFunctionReference(FQName.fqn("morphir.SDK")("decimal")("mul"), returnType, argumentType))
            case "/" =>
              Success(toFunctionReference(FQName.fqn("morphir.SDK")("decimal")("div.unsafe"), returnType, argumentType))
            case "<" =>
              Success(toFunctionReference(FQName.fqn("morphir.SDK")("decimal")("lt"), returnType, argumentType))
            case "<=" =>
              Success(toFunctionReference(FQName.fqn("morphir.SDK")("decimal")("lte"), returnType, argumentType))
            case ">" =>
              Success(toFunctionReference(FQName.fqn("morphir.SDK")("decimal")("gt"), returnType, argumentType))
            case ">=" =>
              Success(toFunctionReference(FQName.fqn("morphir.SDK")("decimal")("gte"), returnType, argumentType))
            case _ => bySymbolNamespace(symbolNamespace, returnType, argumentType)
          }
        } else {
          bySymbolNamespace(symbolNamespace, returnType, argumentType)
        }

      case _ =>
        symbolNamespace match {
          // Constructors
          case "None" :: "scala" :: Nil =>
            Success(toConstructor(FQName.fqn("morphir.SDK")("maybe")("nothing"), returnType))
          case "Some" :: "scala" :: Nil =>
            Success(toConstructor(FQName.fqn("morphir.SDK")("maybe")("just"), returnType))
          // If not implemented, return a failure
          case x => Failure(Exception(s"Constructor for symbol ${symbolNamespace.mkString(",")} not found."))
        }
    }
  }

  private def bySymbolNamespace(
    symbolNamespace: List[String],
    returnType: MorphType.Type[Unit],
    argumentType: MorphType.Type[Unit]
  )(using Quotes)(using Contexts.Context): Try[Value.Value[Unit, MorphType.Type[Unit]]] =
    symbolNamespace match {
      case "length" :: _ if isListType(argumentType) =>
        Success(toUnaryFunctionReference(FQName.fqn("morphir.SDK")("list")("length"), returnType, argumentType))
      case "&&" :: _      => Success(basicsBinary("and", returnType, argumentType))
      case "||" :: _      => Success(basicsBinary("or", returnType, argumentType))
      case "==" :: _      => Success(basicsBinary("equal", returnType, argumentType))
      case "!=" :: _      => Success(basicsBinary("not.equal", returnType, argumentType))
      case "unary_!" :: _ => Success(basicsUnary("not", returnType, argumentType))
      case "unary_-" :: _ => Success(basicsUnary("negate", returnType, argumentType))
      case "abs" :: _     => Success(basicsUnary("abs", returnType, argumentType))
      case "toDouble" :: _ | "toFloat" :: _
          if argumentType == StandardTypes.intReference && returnType == StandardTypes.floatReference =>
        Success(basicsUnary("to.float", returnType, argumentType))
      case "toInt" :: _ if argumentType == StandardTypes.floatReference && returnType == StandardTypes.intReference =>
        Success(basicsUnary("truncate", returnType, argumentType))
      case "min" :: _                     => Success(basicsBinary("min", returnType, argumentType))
      case "max" :: _                     => Success(basicsBinary("max", returnType, argumentType))
      case "/" :: "Int" :: "scala" :: Nil => Success(basicsBinary("integerDivide", returnType, argumentType))
      case "+" :: _                       => Success(basicsBinary("add", returnType, argumentType))
      case "-" :: _                       => Success(basicsBinary("subtract", returnType, argumentType))
      case "*" :: _                       => Success(basicsBinary("multiply", returnType, argumentType))
      case "/" :: _                       => Success(basicsBinary("divide", returnType, argumentType))
      case "<" :: _                       => Success(basicsBinary("less.than", returnType, argumentType))
      case "<=" :: _                      => Success(basicsBinary("less.than.or.equal", returnType, argumentType))
      case ">" :: _                       => Success(basicsBinary("greater.than", returnType, argumentType))
      case ">=" :: _                      => Success(basicsBinary("greater.than.or.equal", returnType, argumentType))
      case x                              => Failure(Exception(s"Standard function for symbol ${symbolNamespace.mkString(",")} not found."))
    }

  private def basicsBinary(
    localName: String,
    returnType: MorphType.Type[Unit],
    argumentType: MorphType.Type[Unit]
  )(using Quotes)(using Contexts.Context): Value.Value.Reference[Unit, MorphType.Type[Unit]] =
    toFunctionReference(basicsPackage(localName), returnType, argumentType)

  private def basicsUnary(
    localName: String,
    returnType: MorphType.Type[Unit],
    argumentType: MorphType.Type[Unit]
  )(using Quotes)(using Contexts.Context): Value.Value.Reference[Unit, MorphType.Type[Unit]] =
    toUnaryFunctionReference(basicsPackage(localName), returnType, argumentType)

  private def toFunctionReference(
    fQName: FQName.FQName,
    returnType: MorphType.Type[Unit],
    argumentType: MorphType.Type[Unit]
  )(using Quotes)(using Contexts.Context): Value.Value.Reference[Unit, MorphType.Type[Unit]] =
    Value.Value.Reference(
      MorphType.Function(
        (),
        argumentType,
        MorphType.Function(
          (),
          argumentType,
          returnType
        )
      ),
      fQName
    )

  private def toUnaryFunctionReference(
    fQName: FQName.FQName,
    returnType: MorphType.Type[Unit],
    argumentType: MorphType.Type[Unit]
  )(using Quotes)(using Contexts.Context): Value.Value.Reference[Unit, MorphType.Type[Unit]] =
    Value.Value.Reference(
      MorphType.Function(
        (),
        argumentType,
        returnType
      ),
      fQName
    )

  private def toCurriedFunctionReference(
    fQName: FQName.FQName,
    returnType: MorphType.Type[Unit],
    argumentTypes: List[MorphType.Type[Unit]]
  )(using Quotes)(using Contexts.Context): Value.Value.Reference[Unit, MorphType.Type[Unit]] =
    Value.Value.Reference(
      argumentTypes.foldRight(returnType) { (argumentType, accType) =>
        MorphType.Function((), argumentType, accType)
      },
      fQName
    )

  private def toConstructor(fQName: FQName.FQName, returnType: MorphType.Type[Unit])(using
    Quotes
  )(using Contexts.Context): Value.Value.Constructor[Unit, MorphType.Type[Unit]] =
    Value.Value.Constructor(
      returnType,
      fQName
    )
}
