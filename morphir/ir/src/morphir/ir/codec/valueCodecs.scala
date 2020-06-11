package morphir.ir.codec

import morphir.ir.{ literal, FQName, Value }
import morphir.ir.Value._
import morphir.ir.core.TaggedCompanionObjectLike
import morphir.ir.json.Decode.DecodeError
import upickle.default._

object valueCodecs {
  trait ValueCodec {
    implicit def readWriter[A: ReadWriter]: ReadWriter[Value[A]] = readwriter[ujson.Value].bimap[Value[A]](
      valueExpr => ujson.Arr(ujson.Str(valueExpr.tag)),
      json => {
        val exprTag = json(0).str
        exprTag match {
          case tag => throw DecodeError.unexpectedTag(tag, Literal.Tag, Constructor.Tag)
        }
      }
    )
  }

  trait LiteralCodec extends TaggedCompanionObjectLike {
    val Tag: String = "literal"

    implicit def readWriter[A: ReadWriter]: ReadWriter[Literal[A]] =
      readwriter[(String, A, literal.Literal)].bimap[Literal[A]](
        valueExpr => (Tag, valueExpr.attributes, valueExpr.value), {
          case (tag, attributes, value: literal.Literal) if tag == Tag => Literal(attributes, value)
          case (tag: String, _, _)                                     => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait ConstructorCodec extends TaggedCompanionObjectLike {
    val Tag: String = "constructor"

    implicit def readWriter[A: ReadWriter]: ReadWriter[Constructor[A]] =
      readwriter[(String, A, FQName)].bimap(
        ctor => (Tag, ctor.attributes, ctor.fullyQualifiedName), {
          case (tag, attributes, fullyQualifiedName) if tag == Tag => Constructor(attributes, fullyQualifiedName)
          case (tag, _, _)                                         => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait TupleCodec extends TaggedCompanionObjectLike {
    val Tag: String = "tuple"
  }

  trait ListCodec extends TaggedCompanionObjectLike {
    val Tag: String = "list"
  }

  trait RecordCodec extends TaggedCompanionObjectLike {
    val Tag: String = "record"
  }

  trait VariableCodec extends TaggedCompanionObjectLike {
    val Tag: String = "variable"
  }

  trait ReferenceCodec extends TaggedCompanionObjectLike {
    val Tag: String = "reference"
  }

  trait FieldCodec extends TaggedCompanionObjectLike {
    val Tag: String = "field"
  }

  trait FieldFunctionCodec extends TaggedCompanionObjectLike {
    val Tag: String = "field_function"
  }

  trait ApplyCodec extends TaggedCompanionObjectLike {
    val Tag: String = "apply"
  }

  trait LambdaCodec extends TaggedCompanionObjectLike {
    val Tag: String = "lambda"
  }

  trait LetDefinitionCodec extends TaggedCompanionObjectLike {
    val Tag: String = "let_definition"
  }

  trait LetRecursionCodec extends TaggedCompanionObjectLike {
    val Tag: String = "let_recursion"
  }

  trait DestructureCodec extends TaggedCompanionObjectLike {
    val Tag: String = "destructure"
  }

  trait IfThenElseCodec extends TaggedCompanionObjectLike {
    val Tag: String = "if_then_else"
  }

  trait PatternMatchCodec extends TaggedCompanionObjectLike {
    val Tag: String = "pattern_match"
  }

  trait UpdateRecordCodec extends TaggedCompanionObjectLike {
    val Tag: String = "update_record"
  }

  trait UnitCodec extends TaggedCompanionObjectLike {
    val Tag: String = "unit"

    implicit def readWriter[A: ReadWriter]: ReadWriter[Unit[A]] =
      readwriter[(String, A)].bimap(
        expr => (Tag, expr.attributes), {
          case (tag, attributes) if tag == Tag => Value.Unit(attributes)
          case (tag: String, _)                => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait DefinitionCodec extends TaggedCompanionObjectLike {
    val Tag: String = "definition"

    implicit def readWriter[A: ReadWriter]: ReadWriter[Definition[A]] =
      readwriter[ujson.Value].bimap(
        defn => {
          val valueType = defn.valueType.fold[ujson.Value](ujson.Null)(tpe => writeJs(tpe))
          val arguments = writeJs(defn.arguments)
          val body      = writeJs(defn.body)
          ujson.Arr(ujson.Str(Tag), valueType, arguments, body)
        },
        _ => ???
      )
//      readwriter[(String, Option[Type[A]], ArgumentList[A], Value[A])].bimap[Definition[A]](
//        defn => (Tag, defn.valueType, defn.arguments, defn.body), {
//          case (tag, valueType: Option[_], arguments: ArgumentList[_], body: Value[_]) if tag == Tag =>
//            Definition(valueType, arguments, body)
//          case (tag: String, _, _, _) => throw DecodeError.unexpectedTag(tag, Tag)
//        }
//      )
  }
}
