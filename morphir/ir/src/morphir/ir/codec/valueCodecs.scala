package morphir.ir.codec

import morphir.ir.{ literal, pattern, FQName, Type, Value }
import morphir.ir.name.Name
import morphir.ir.Value._
import morphir.ir.argument.Argument
import morphir.ir.core.TaggedCompanionObjectLike
import morphir.ir.json.Decode.DecodeError
import upickle.default._

// scalafix:off DisableSyntax.throw
object valueCodecs {
  trait ValueCodec {
    implicit def readWriter[A: ReadWriter]: ReadWriter[Value[A]] = readwriter[ujson.Value].bimap[Value[A]](
      {
        case expr @ Literal(_, _)             => writeJs(expr)
        case expr @ Constructor(_, _)         => writeJs(expr)
        case expr @ Tuple(_, _)               => writeJs(expr)
        case expr @ List(_, _)                => writeJs(expr)
        case expr @ Record(_, _)              => writeJs(expr)
        case expr @ Variable(_, _)            => writeJs(expr)
        case expr @ Reference(_, _)           => writeJs(expr)
        case expr @ Field(_, _, _)            => writeJs(expr)
        case expr @ FieldFunction(_, _)       => writeJs(expr)
        case expr @ Apply(_, _, _)            => writeJs(expr)
        case expr @ Lambda(_, _, _)           => writeJs(expr)
        case expr @ LetDefinition(_, _, _, _) => writeJs(expr)
        case expr @ LetRecursion(_, _, _)     => writeJs(expr)
        case expr @ Destructure(_, _, _, _)   => writeJs(expr)
        case expr @ IfThenElse(_, _, _, _)    => writeJs(expr)
        case expr @ PatternMatch(_, _, _)     => writeJs(expr)
        case expr @ UpdateRecord(_, _, _)     => writeJs(expr)
        case expr @ Unit(_)                   => writeJs(expr)
      },
      json => {
        val exprTag = json(0).str
        exprTag match {
          case tag if tag == Literal.Tag       => read[Literal[A]](json)
          case tag if tag == Constructor.Tag   => read[Constructor[A]](json)
          case tag if tag == Tuple.Tag         => read[Tuple[A]](json)
          case tag if tag == List.Tag          => read[List[A]](json)
          case tag if tag == Record.Tag        => read[Record[A]](json)
          case tag if tag == Variable.Tag      => read[Variable[A]](json)
          case tag if tag == Reference.Tag     => read[Reference[A]](json)
          case tag if tag == Field.Tag         => read[Field[A]](json)
          case tag if tag == FieldFunction.Tag => read[FieldFunction[A]](json)
          case tag if tag == Apply.Tag         => read[Apply[A]](json)
          case tag if tag == Lambda.Tag        => read[Lambda[A]](json)
          case tag if tag == LetDefinition.Tag => read[LetDefinition[A]](json)
          case tag if tag == LetRecursion.Tag  => read[LetRecursion[A]](json)
          case tag if tag == Destructure.Tag   => read[Destructure[A]](json)
          case tag if tag == IfThenElse.Tag    => read[IfThenElse[A]](json)
          case tag if tag == PatternMatch.Tag  => read[PatternMatch[A]](json)
          case tag if tag == UpdateRecord.Tag  => read[UpdateRecord[A]](json)
          case tag if tag == Unit.Tag          => read[Unit[A]](json)
          case tag =>
            println(s"[BAD|JSON]: $json")
            throw DecodeError
              .unexpectedTag(
                tag,
                Literal.Tag,
                Constructor.Tag,
                Tuple.Tag,
                List.Tag,
                Record.Tag,
                Variable.Tag,
                Reference.Tag,
                Field.Tag,
                FieldFunction.Tag,
                Apply.Tag,
                Lambda.Tag,
                LetDefinition.Tag,
                LetRecursion.Tag,
                Destructure.Tag,
                IfThenElse.Tag,
                PatternMatch.Tag,
                UpdateRecord.Tag,
                Unit.Tag
              )
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

    implicit def readWriter[A: ReadWriter]: ReadWriter[Tuple[A]] =
      readwriter[(String, A, scala.List[Value[A]])].bimap(
        expr => (expr.tag, expr.attributes, expr.elements), {
          case (tag, attributes, elements) if tag == Tag => Tuple(attributes, elements)
          case (tag, _, _)                               => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait ListCodec extends TaggedCompanionObjectLike {
    val Tag: String = "list"

    implicit def readWriter[A: ReadWriter]: ReadWriter[List[A]] =
      readwriter[(String, A, scala.List[Value[A]])].bimap(
        expr => (expr.tag, expr.attributes, expr.items), {
          case (tag, attributes, items) if tag == Tag => List(attributes, items)
          case (tag, _, _)                            => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait RecordCodec extends TaggedCompanionObjectLike {
    val Tag: String = "record"

    implicit def readWriter[A: ReadWriter]: ReadWriter[Record[A]] =
      readwriter[(String, A, scala.List[(Name, Value[A])])].bimap(
        expr => (expr.tag, expr.attributes, expr.fields), {
          case (tag, attributes, fields) if tag == Tag => Record(attributes, fields)
          case (tag, _, _)                             => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait VariableCodec extends TaggedCompanionObjectLike {
    val Tag: String = "variable"

    implicit def readWriter[A: ReadWriter]: ReadWriter[Variable[A]] =
      readwriter[(String, A, Name)].bimap(
        expr => (expr.tag, expr.attributes, expr.name), {
          case (tag, attributes, name) if tag == Tag => Variable(attributes, name)
          case (tag, _, _)                           => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait ReferenceCodec extends TaggedCompanionObjectLike {
    val Tag: String = "reference"

    implicit def readWriter[A: ReadWriter]: ReadWriter[Reference[A]] =
      readwriter[(String, A, FQName)].bimap(
        expr => (expr.tag, expr.attributes, expr.fullyQualifiedName), {
          case (tag, attributes, fullyQualifiedName) if tag == Tag => Reference(attributes, fullyQualifiedName)
          case (tag, _, _)                                         => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait FieldCodec extends TaggedCompanionObjectLike {
    val Tag: String = "field"

    implicit def readWriter[A: ReadWriter]: ReadWriter[Field[A]] =
      readwriter[(String, A, Value[A], Name)].bimap(
        expr => (expr.tag, expr.attributes, expr.subjectValue, expr.fieldName), {
          case (tag, attributes, subjectValue, fieldName) if tag == Tag => Field(attributes, subjectValue, fieldName)
          case (tag, _, _, _)                                           => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait FieldFunctionCodec extends TaggedCompanionObjectLike {
    val Tag: String = "field_function"

    implicit def readWriter[A: ReadWriter]: ReadWriter[FieldFunction[A]] =
      readwriter[(String, A, Name)].bimap(
        expr => (expr.tag, expr.attributes, expr.fieldName), {
          case (tag, attributes, fieldName) if tag == Tag => FieldFunction(attributes, fieldName)
          case (tag, _, _)                                => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait ApplyCodec extends TaggedCompanionObjectLike {
    val Tag: String = "apply"

    implicit def readWriter[A: ReadWriter]: ReadWriter[Apply[A]] =
      readwriter[(String, A, Value[A], Value[A])].bimap(
        expr => (expr.tag, expr.attributes, expr.function, expr.argument), {
          case (tag, attributes, function, argument) if tag == Tag => Apply(attributes, function, argument)
          case (tag, _, _, _)                                      => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait LambdaCodec extends TaggedCompanionObjectLike {
    val Tag: String = "lambda"

    implicit def readWriter[A: ReadWriter]: ReadWriter[Lambda[A]] =
      readwriter[(String, A, pattern.Pattern[A], Value[A])].bimap[Lambda[A]](
        expr => (expr.tag, expr.attributes, expr.argumentPattern, expr.body), {
          case (tag, attributes, argumentPattern, body) if tag == Tag => Lambda(attributes, argumentPattern, body)
          case (tag, _, _, _)                                         => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait LetDefinitionCodec extends TaggedCompanionObjectLike {
    val Tag: String = "let_definition"

    implicit def readWriter[A: ReadWriter]: ReadWriter[LetDefinition[A]] =
      readwriter[(String, A, Name, Definition[A], Value[A])].bimap(
        expr => (expr.tag, expr.attributes, expr.valueName, expr.valueDefinition, expr.inValue), {
          case (tag, attributes, valueName, valueDefinition, inValue) if tag == Tag =>
            LetDefinition(attributes, valueName, valueDefinition, inValue)
          case (tag, _, _, _, _) => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )

  }

  trait LetRecursionCodec extends TaggedCompanionObjectLike {
    val Tag: String = "let_recursion"

    implicit def readWriter[A: ReadWriter]: ReadWriter[LetRecursion[A]] =
      readwriter[(String, A, scala.List[(Name, Definition[A])], Value[A])].bimap(
        expr => {
          val valDefs = expr.valueDefinitions.toList
          (expr.tag, expr.attributes, valDefs, expr.inValue)
        }, {
          case (tag, attributes, valueDefinitions, inValue) if tag == Tag =>
            LetRecursion(attributes, valueDefinitions.toMap, inValue)
          case (tag, _, _, _) => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait DestructureCodec extends TaggedCompanionObjectLike {
    val Tag: String = "destructure"

    implicit def readWriter[A: ReadWriter]: ReadWriter[Destructure[A]] =
      readwriter[(String, A, pattern.Pattern[A], Value[A], Value[A])].bimap(
        expr => (expr.tag, expr.attributes, expr.pattern, expr.valueToDestruct, expr.inValue), {
          case (tag, attributes, pattern, valueToDestruct, inValue) if tag == Tag =>
            Destructure(attributes, pattern, valueToDestruct, inValue)
          case (tag, _, _, _, _) => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait IfThenElseCodec extends TaggedCompanionObjectLike {
    val Tag: String = "if_then_else"

    implicit def readWriter[A: ReadWriter]: ReadWriter[IfThenElse[A]] =
      readwriter[(String, A, Value[A], Value[A], Value[A])].bimap(
        expr => (expr.tag, expr.attributes, expr.condition, expr.thenBranch, expr.elseBranch), {
          case (tag, attributes, condition, thenBranch, elseBranch) if tag == Tag =>
            IfThenElse(attributes, condition, thenBranch, elseBranch)
          case (tag, _, _, _, _) => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait PatternMatchCodec extends TaggedCompanionObjectLike {
    val Tag: String = "pattern_match"

    implicit def readWriter[A: ReadWriter]: ReadWriter[PatternMatch[A]] =
      readwriter[(String, A, Value[A], scala.List[(pattern.Pattern[A], Value[A])])].bimap(
        expr => (expr.tag, expr.attributes, expr.branchOutOn, expr.cases), {
          case (tag, attributes, branchOutOn, cases) if tag == Tag => PatternMatch(attributes, branchOutOn, cases)
          case (tag, _, _, _)                                      => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait UpdateRecordCodec extends TaggedCompanionObjectLike {
    val Tag: String = "update_record"

    implicit def readWriter[A: ReadWriter]: ReadWriter[UpdateRecord[A]] =
      readwriter[(String, A, Value[A], scala.List[(Name, Value[A])])].bimap(
        expr => (expr.tag, expr.attributes, expr.valueToUpdate, expr.fieldsToUpdate), {
          case (tag, attributes, valueToUpdate, fieldsToUpdate) if tag == Tag =>
            UpdateRecord(attributes, valueToUpdate, fieldsToUpdate)
          case (tag, _, _, _) => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
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
        json => {
          val tag = json(0).str
          if (tag == Tag) {
            val valueType = read[Option[Type[A]]](json(1))
            val arguments = read[scala.List[Argument[A]]](json(2))
            val body      = read[Value[A]](json(3))
            Definition(valueType, arguments, body)
          } else {
            throw DecodeError.unexpectedTag(tag, Tag)
          }
        }
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
// scalafix:on
