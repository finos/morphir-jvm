package morphir.ir.codec

import morphir.ir.{ AccessControlled, FQName, Type }
import morphir.ir.name.Name
import morphir.ir.Type._
import morphir.ir.Type.Definition._
import morphir.ir.core.TaggedCompanionObjectLike
import morphir.ir.json.Decode.DecodeError
import upickle.default
import upickle.default._

object typeCodecs {

  trait TypeCodec {
    implicit def readWriter[A: ReadWriter]: ReadWriter[Type[A]] =
      readwriter[ujson.Value].bimap[Type[A]](
        {
          case tpe @ Variable(_, _)            => writeJs(tpe)
          case tpe @ Reference(_, _, _)        => writeJs(tpe)
          case tpe @ Tuple(_, _)               => writeJs(tpe)
          case tpe @ Record(_, _)              => writeJs(tpe)
          case tpe @ ExtensibleRecord(_, _, _) => writeJs(tpe)
          case tpe @ Function(_, _, _)         => writeJs(tpe)
          case tpe @ Unit(_)                   => writeJs(tpe)
        },
        json => {
          val typeExprTag = json(0).str
          typeExprTag match {
            case tag if tag == Variable.Tag         => read[Variable[A]](json)
            case tag if tag == Reference.Tag        => read[Reference[A]](json)
            case tag if tag == Tuple.Tag            => read[Tuple[A]](json)
            case tag if tag == Record.Tag           => read[Record[A]](json)
            case tag if tag == ExtensibleRecord.Tag => read[ExtensibleRecord[A]](json)
            case tag if tag == Function.Tag         => read[Function[A]](json)
            case tag if tag == Unit.Tag             => read[Unit[A]](json)
            case tag =>
              throw DecodeError.unexpectedTag(
                tag,
                Variable.Tag,
                Reference.Tag,
                Tuple.Tag,
                Record.Tag,
                ExtensibleRecord.Tag,
                Function.Tag,
                Unit.Tag
              )
          }
        }
      )
  }

  trait DefinitionCodec {
    implicit def readWriter[A: ReadWriter]: ReadWriter[Definition[A]] =
      readwriter[ujson.Value].bimap[Definition[A]](
        {
          case definition @ TypeAliasDefinition(_, _)  => writeJs(definition)
          case definition @ CustomTypeDefinition(_, _) => writeJs(definition)
        },
        json => {
          val tag = json(0).str
          tag match {
            case "type_alias_definition"  => read[TypeAliasDefinition[A]](json)
            case "custom_type_definition" => read[CustomTypeDefinition[A]](json)
            case _                        => throw DecodeError.unexpectedTag(tag, "type_alias_definition", "custom_type_definition")
          }
        }
      )
  }

  trait TypeAliasDefinitionCodec {
    implicit def readWriter[A: ReadWriter]: ReadWriter[TypeAliasDefinition[A]] =
      readwriter[ujson.Value].bimap[TypeAliasDefinition[A]](
        defn =>
          ujson.Arr(
            ujson.Str("type_alias_definition"),
            writeJs(defn.typeParams),
            ujson.Null
          ),
        json =>
          //println(s"Type Alias Def JSON: $json")
          json(0).str match {
            case "type_alias_definition" =>
              val typeParams = read[List[Name]](json(1))
              println(s"typeParams: $typeParams")
              val typeExp = read[Type[A]](json(2))
              TypeAliasDefinition(typeParams, typeExp)
            case tag => throw DecodeError.unexpectedTag(tag, "type_alias_definition")
          }
      )
  }

  trait CustomTypeDefinitionCodec {
    implicit def readWriter[A: ReadWriter]: ReadWriter[CustomTypeDefinition[A]] =
      readwriter[ujson.Value].bimap[CustomTypeDefinition[A]](
        defn =>
          ujson.Arr(
            ujson.Str("custom_type_definition"),
            writeJs(defn.typeParams),
            ujson.Null
          ),
        json =>
          json(0).str match {
            case "custom_type_definition" =>
              val typeParams = read[scala.List[Name]](json(1))
              val ctors      = read[AccessControlled[Constructors[A]]](json(2))
              CustomTypeDefinition(typeParams, ctors)
            case tag => throw DecodeError.unexpectedTag(tag, "custom_type_definition")
          }
      )
  }

  trait VariableCodec extends TaggedCompanionObjectLike {
    val Tag: String = "variable"

    implicit def readWriter[A: ReadWriter]: ReadWriter[Variable[A]] = readwriter[(String, A, Name)].bimap[Variable[A]](
      typeExpr => (Tag, typeExpr.attributes, typeExpr.name), {
        case (tag, attributes, name) if tag == Tag => Variable(attributes, name)
        case (tag, _, _)                           => throw DecodeError.unexpectedTag(tag, Tag)
      }
    )
  }

  trait ReferenceCodec extends TaggedCompanionObjectLike {
    val Tag: String = "reference"

    implicit def readWriter[A: ReadWriter]: ReadWriter[Reference[A]] =
      readwriter[(String, A, FQName, List[Type[A]])].bimap[Reference[A]](
        typeExpr => (Tag, typeExpr.attributes, typeExpr.typeName, typeExpr.typeParameters), {
          case (tag, attributes, typeName, typeParameters) if tag == Tag =>
            Reference(attributes, typeName, typeParameters)
          case (tag, _, _, _) => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait TupleCodec extends TaggedCompanionObjectLike {
    val Tag: String = "tuple"

    implicit def readWriter[A: ReadWriter]: ReadWriter[Tuple[A]] =
      readwriter[(String, A, scala.List[Type[A]])].bimap[Tuple[A]](
        typeExpr => (Tag, typeExpr.attributes, typeExpr.elementTypes), {
          case (tag, attributes, elementTypes) if tag == Tag => Tuple(attributes, elementTypes)
          case (tag, _, _)                                   => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait RecordCodec extends TaggedCompanionObjectLike {
    val Tag: String = "record"

    implicit def readWriter[A: ReadWriter]: default.ReadWriter[Record[A]] =
      readwriter[(String, A, scala.List[Field[A]])].bimap[Record[A]](
        rec => (Tag, rec.attributes, rec.fieldTypes), {
          case (tag, attributes, fieldTypes) if tag == Tag => Record(attributes, fieldTypes)
          case (tag, _, _)                                 => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )

  }

  trait ExtensibleRecordCodec extends TaggedCompanionObjectLike {
    val Tag: String = "extensible_record"

    implicit def readWriter[A: ReadWriter]: ReadWriter[ExtensibleRecord[A]] =
      readwriter[(String, A, Name, scala.List[Field[A]])].bimap[ExtensibleRecord[A]](
        typExpr => (Tag, typExpr.attributes, typExpr.variableName, typExpr.fieldTypes), {
          case (tag, attributes, variableName, fieldTypes) if tag == Tag =>
            ExtensibleRecord(attributes, variableName, fieldTypes)
          case (tag, _, _, _) => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )

  }

  trait FunctionCodec extends TaggedCompanionObjectLike {
    val Tag: String = "function"

    implicit def readWriter[A: ReadWriter]: ReadWriter[Function[A]] =
      readwriter[(String, A, Type[A], Type[A])].bimap[Function[A]](
        typeExpr => (Tag, typeExpr.attributes, typeExpr.argumentType, typeExpr.returnType), {
          case (tag, attributes, argumentType, returnType) if tag == Tag =>
            Function(attributes, argumentType, returnType)
          case (tag, _, _, _) => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait UnitCodec extends TaggedCompanionObjectLike {
    val Tag: String = "unit"

    implicit def readWriter[A: ReadWriter]: ReadWriter[Unit[A]] =
      readwriter[(String, A)].bimap[Unit[A]](
        (typeExpr: Unit[A]) => (Tag, typeExpr.attributes), {
          case ("unit", attributes) => Unit(attributes)
          case (tag, _)             => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )

  }

  trait FieldCodec {
    implicit def readWriter[A: ReadWriter]: ReadWriter[Field[A]] = readwriter[(Name, Type[A])].bimap[Field[A]](
      field => (field.name, field.fieldType), {
        case (name, fieldType) => Field(name, fieldType)
      }
    )

  }

  trait ConstructorCodec extends TaggedCompanionObjectLike {
    val Tag: String = "constructor"

    implicit def readWriter[A: ReadWriter]: ReadWriter[Constructor[A]] =
      readwriter[(String, Name, scala.List[(Name, Type[A])])].bimap[Constructor[A]](
        ctor => (Tag, ctor.name, ctor.args), {
          case (tag, name, args) if tag == Tag => Constructor(name, args)
          case (tag, _, _)                     => throw DecodeError.unexpectedTag(tag, Tag)
        }
      )
  }

  trait ConstructorsCodec {

    implicit def readWriter[A: ReadWriter]: ReadWriter[Constructors[A]] =
      readwriter[scala.List[Constructor[A]]].bimap(
        ctors => ctors.toList,
        ctors => Constructors(ctors)
      )
  }
}
