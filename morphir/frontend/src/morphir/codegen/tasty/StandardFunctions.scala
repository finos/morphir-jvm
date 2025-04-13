package morphir.codegen.tasty


import dotty.tools.dotc.core.{Contexts, Names}
import morphir.ir.{Type as MorphType, *}

import scala.quoted.Quotes
import scala.util.{Success, Try}

object StandardFunctions {

  def get(name: Names.Name, returnType: MorphType.Type[Unit], argumentType: MorphType.Type[Unit])(using Quotes)(using Contexts.Context): Try[Value.Value.Reference[Unit, MorphType.Type[Unit]]] = {
    val maybeStandardFunction = name.show match {
      case "+" => Success(FQName.fqn("morphir.SDK")("basics")("add"))
      case "-" => Success(FQName.fqn("morphir.SDK")("basics")("subtract"))
      case "*" => Success(FQName.fqn("morphir.SDK")("basics")("multiply"))
      case "/" if argumentType == StandardTypes.intReference => Success(FQName.fqn("morphir.SDK")("basics")("integerDivide"))
      case "/" => Success(FQName.fqn("morphir.SDK")("basics")("divide"))
    }

    maybeStandardFunction.map { fQName =>
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
    }
  }
}
