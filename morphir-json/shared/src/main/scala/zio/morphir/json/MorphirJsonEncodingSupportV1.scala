package zio.morphir.json

import zio._
import zio.json._
import zio.json.ast.Json
import zio.morphir.ir._
import zio.morphir.ir.AccessControlled.Access._
import zio.morphir.ir.Literal
import zio.morphir.ir.ValueModule.{Value, ValueCase}
import zio.morphir.ir.TypeModule._
import zio.json.internal.Write
import zio.morphir.ir.TypeModule.Type.{Record, Reference, ExtensibleRecord, Variable, Tuple}

trait MorphirJsonEncodingSupportV1 {
  // NOTE: We will want to create JSON encoders which follow the format in the morphir-elm project
  implicit val unitEncoder: JsonEncoder[Unit] = JsonEncoder.list[String].contramap(_ => List.empty[String])
  implicit val nameEncoder: JsonEncoder[Name] = JsonEncoder.list[String].contramap(name => name.toList)
  implicit val pathEncoder: JsonEncoder[Path] = JsonEncoder.list[Name].contramap(path => path.segments.toList)
  implicit val modulePathEncoder: JsonEncoder[ModulePath]   = pathEncoder.contramap(_.toPath)
  implicit val packageNameEncoder: JsonEncoder[PackageName] = pathEncoder.contramap(_.toPath)
  implicit val qNameEncoder: JsonEncoder[QName] =
    Json.encoder.contramap[QName](qName =>
      Json.Arr(toJsonAstOrThrow(qName.modulePath), toJsonAstOrThrow(qName.localName))
    )

  implicit val fqNameEncoder: JsonEncoder[FQName] =
    Json.encoder.contramap[FQName](fqName =>
      Json.Arr(
        toJsonAstOrThrow(fqName.packagePath),
        toJsonAstOrThrow(fqName.modulePath),
        toJsonAstOrThrow(fqName.localName)
      )
    )

  implicit val moduleNameEncoder: JsonEncoder[ModuleModule.ModuleName] =
    Json.encoder.contramap[ModuleModule.ModuleName](moduleName =>
      Json.Arr(toJsonAstOrThrow(moduleName.namespace), toJsonAstOrThrow(moduleName.localName))
    )

  implicit def fieldEncoder[A](implicit encoder: JsonEncoder[A]): JsonEncoder[Field[A]] =
    Json.encoder.contramap[Field[A]](field => Json.Arr(toJsonAstOrThrow(field.name), toJsonAstOrThrow(field.fieldType)))

  implicit def literalBoolEncoder: JsonEncoder[Literal.Bool] = Json.encoder.contramap[Literal.Bool] { literal =>
    Json.Arr(Json.Str("bool_literal"), Json.Bool(literal.value))
  }
  implicit def literalCharEncoder: JsonEncoder[Literal.Char] = Json.encoder.contramap[Literal.Char] { literal =>
    Json.Arr(Json.Str("char_literal"), Json.Str(literal.value.toString))
  }
  implicit def literalFloatEncoder: JsonEncoder[Literal.Float] = Json.encoder.contramap[Literal.Float] { literal =>
    Json.Arr(Json.Str("float_literal"), Json.Num(literal.value))
  }
  implicit def literalStringEncoder: JsonEncoder[Literal.String] = Json.encoder.contramap[Literal.String] { literal =>
    Json.Arr(Json.Str("string_literal"), Json.Str(literal.value))
  }
  implicit def literalWholeNumberEncoder: JsonEncoder[Literal.WholeNumber] =
    Json.encoder.contramap[Literal.WholeNumber] { literal =>
      Json.Arr(Json.Str("int_literal"), Json.Num(new java.math.BigDecimal(literal.value)))
    }

  implicit def literalEncoder[Attributes: JsonEncoder]: JsonEncoder[Literal[Attributes]] =
    new JsonEncoder[Literal[Attributes]] {
      def unsafeEncode(a: Literal[Attributes], indent: Option[Int], out: Write): Unit = a match {
        case literalBool: Literal.Bool     => literalBoolEncoder.unsafeEncode(literalBool, indent, out)
        case literalChar: Literal.Char     => literalCharEncoder.unsafeEncode(literalChar, indent, out)
        case literalFloat: Literal.Float   => literalFloatEncoder.unsafeEncode(literalFloat, indent, out)
        case literalString: Literal.String => literalStringEncoder.unsafeEncode(literalString, indent, out)
        case literalWholeNumber: Literal.WholeNumber =>
          literalWholeNumberEncoder.unsafeEncode(literalWholeNumber, indent, out)
      }
    }

  implicit def patternAsPatternEncoder[Attributes: JsonEncoder]: JsonEncoder[Pattern.AsPattern[Attributes]] =
    JsonEncoder.tuple4[String, Attributes, Pattern[Attributes], Name].contramap {
      case Pattern.AsPattern(pattern, name, attributes) =>
        ("as_pattern", attributes, pattern, name)
    }

  implicit def patternConstructorPatternEncoder[Attributes: JsonEncoder]
      : JsonEncoder[Pattern.ConstructorPattern[Attributes]] =
    JsonEncoder.tuple4[String, Attributes, FQName, Chunk[Pattern[Attributes]]].contramap {
      case Pattern.ConstructorPattern(constructorName, argumentPatterns, attributes) =>
        ("constructor_pattern", attributes, constructorName, argumentPatterns)
    }

  implicit def patternEmptyListPatternEncoder[Attributes: JsonEncoder]
      : JsonEncoder[Pattern.EmptyListPattern[Attributes]] =
    JsonEncoder.tuple2[String, Attributes].contramap { case Pattern.EmptyListPattern(attributes) =>
      ("empty_list_pattern", attributes)
    }

  implicit def patternHeadTailPatternEncoder[Attributes: JsonEncoder]
      : JsonEncoder[Pattern.HeadTailPattern[Attributes]] =
    JsonEncoder.tuple4[String, Attributes, Pattern[Attributes], Pattern[Attributes]].contramap {
      case Pattern.HeadTailPattern(headPattern, tailPattern, attributes) =>
        ("head_tail_pattern", attributes, headPattern, tailPattern)
    }

  implicit def patternLiteralPatternEncoder[A, Attributes: JsonEncoder](implicit
      encoder: JsonEncoder[A]
  ): JsonEncoder[Pattern.LiteralPattern[A, Attributes]] =
    JsonEncoder.tuple3[String, Attributes, Literal[A]].contramap { case Pattern.LiteralPattern(literal, attributes) =>
      ("literal_pattern", attributes, literal)
    }

  implicit def patternTuplePatternEncoder[Attributes: JsonEncoder]: JsonEncoder[Pattern.TuplePattern[Attributes]] =
    JsonEncoder.tuple3[String, Attributes, Chunk[Pattern[Attributes]]].contramap {
      case Pattern.TuplePattern(elementPatterns, attributes) =>
        ("tuple_pattern", attributes, elementPatterns)
    }

  implicit def patternUnitPatternEncoder[Attributes: JsonEncoder]: JsonEncoder[Pattern.UnitPattern[Attributes]] =
    JsonEncoder.tuple2[String, Attributes].contramap { case Pattern.UnitPattern(attributes) =>
      ("unit_pattern", attributes)
    }

  implicit def patternWildcardPatternEncoder[Attributes: JsonEncoder]
      : JsonEncoder[Pattern.WildcardPattern[Attributes]] =
    JsonEncoder.tuple2[String, Attributes].contramap { case Pattern.WildcardPattern(attributes) =>
      ("wildcard_pattern", attributes)
    }

  implicit def patternEncoder[Attributes: JsonEncoder]: JsonEncoder[Pattern[Attributes]] =
    new JsonEncoder[Pattern[Attributes]] {
      def unsafeEncode(pattern: Pattern[Attributes], indent: Option[Int], out: Write): Unit = pattern match {
        case pattern @ Pattern.AsPattern(_, _, _) =>
          JsonEncoder[Pattern.AsPattern[Attributes]].unsafeEncode(pattern, indent, out)
        case pattern @ Pattern.ConstructorPattern(_, _, _) =>
          JsonEncoder[Pattern.ConstructorPattern[Attributes]].unsafeEncode(pattern, indent, out)
        case pattern @ Pattern.EmptyListPattern(_) =>
          JsonEncoder[Pattern.EmptyListPattern[Attributes]].unsafeEncode(pattern, indent, out)
        case pattern @ Pattern.HeadTailPattern(_, _, _) =>
          JsonEncoder[Pattern.HeadTailPattern[Attributes]].unsafeEncode(pattern, indent, out)
        case Pattern.LiteralPattern(_, _) => ???
        //   JsonEncoder[Pattern.LiteralPattern[???,Attributes]].unsafeEncode(pattern, indent, out)
        case pattern @ Pattern.TuplePattern(_, _) =>
          JsonEncoder[Pattern.TuplePattern[Attributes]].unsafeEncode(pattern, indent, out)
        case pattern @ Pattern.UnitPattern(_) =>
          JsonEncoder[Pattern.UnitPattern[Attributes]].unsafeEncode(pattern, indent, out)
        case pattern @ Pattern.WildcardPattern(_) =>
          JsonEncoder[Pattern.WildcardPattern[Attributes]].unsafeEncode(pattern, indent, out)
      }
    }

  implicit def constructorsEncoder[Attributes](implicit
      attributesEncoder: JsonEncoder[Attributes]
  ): JsonEncoder[Constructors[Attributes]] = {
    Json.encoder.contramap[Constructors[Attributes]] { ctors =>
      toJsonAstOrThrow(
        ctors.toMap.toList.map { case (ctorName: Name, ctorArgs: Chunk[(Name, Type[Attributes])]) =>
          (
            toJsonAstOrThrow(ctorName),
            toJsonAstOrThrow(
              ctorArgs.map { case (argName: Name, argType: Type[Attributes]) =>
                Json.Arr(toJsonAstOrThrow(argName), toJsonAstOrThrow(argType))
              }
            )
          )
        }
      )
    }
  }

  implicit def accessControlledEncoder[A](implicit encoder: JsonEncoder[A]): JsonEncoder[AccessControlled[A]] =
    Json.encoder.contramap[AccessControlled[A]] { accessControlled =>
      accessControlled.access match {
        case Public  => Json.Arr(Json.Str("public"), toJsonAstOrThrow(accessControlled.value))
        case Private => Json.Arr(Json.Str("private"), toJsonAstOrThrow(accessControlled.value))
      }
    }

  implicit def typeDefinitionTypeAliasEncoder[Attributes](implicit
      attributesEncoder: JsonEncoder[Attributes]
  ): JsonEncoder[TypeModule.Definition.TypeAlias[Attributes]] =
    Json.encoder.contramap[TypeModule.Definition.TypeAlias[Attributes]] { alias =>
      Json.Arr(Json.Str("type_alias_definition"), toJsonAstOrThrow(alias.typeParams), toJsonAstOrThrow(alias.typeExp))
    }

  implicit def typeDefinitionCustomTypeEncoder[Attributes](implicit
      attributesEncoder: JsonEncoder[Attributes]
  ): JsonEncoder[TypeModule.Definition.CustomType[Attributes]] =
    Json.encoder.contramap[TypeModule.Definition.CustomType[Attributes]] { tpe =>
      Json.Arr(Json.Str("custom_type_definition"), toJsonAstOrThrow(tpe.typeParams), toJsonAstOrThrow(tpe.ctors))
    }

  implicit def typeDefinitionEncoder[Attributes: JsonEncoder]: JsonEncoder[TypeModule.Definition[Attributes]] =
    new JsonEncoder[TypeModule.Definition[Attributes]] {
      def unsafeEncode(d: TypeModule.Definition[Attributes], indent: Option[Int], out: Write): Unit = d match {
        case d @ TypeModule.Definition.TypeAlias(_, _) =>
          JsonEncoder[TypeModule.Definition.TypeAlias[Attributes]].unsafeEncode(d, indent, out)
        case d @ TypeModule.Definition.CustomType(_, _) =>
          JsonEncoder[TypeModule.Definition.CustomType[Attributes]].unsafeEncode(d, indent, out)
      }
    }

  implicit def typeSpecificationTypeAliasEncoder[Attributes](implicit
      attributesEncoder: JsonEncoder[Attributes]
  ): JsonEncoder[TypeModule.Specification.TypeAliasSpecification[Attributes]] =
    Json.encoder.contramap[TypeModule.Specification.TypeAliasSpecification[Attributes]] { alias =>
      Json.Arr(Json.Str("type_alias_specification"), toJsonAstOrThrow(alias.typeParams), toJsonAstOrThrow(alias.expr))
    }

  implicit def typeSpecificationEncoderCustomTypeEncoder[Attributes](implicit
      attributesEncoder: JsonEncoder[Attributes]
  ): JsonEncoder[TypeModule.Specification.CustomTypeSpecification[Attributes]] =
    Json.encoder.contramap[TypeModule.Specification.CustomTypeSpecification[Attributes]] { tpe =>
      Json.Arr(Json.Str("custom_type_specification"), toJsonAstOrThrow(tpe.typeParams), toJsonAstOrThrow(tpe.ctors))
    }

  implicit def typeSpecificationEncoderOpaqueTypeEncoder
      : JsonEncoder[TypeModule.Specification.OpaqueTypeSpecification] =
    Json.encoder.contramap[TypeModule.Specification.OpaqueTypeSpecification] { tpe =>
      Json.Arr(Json.Str("opaque_type_specification"), toJsonAstOrThrow(tpe.typeParams))
    }

  implicit def typeSpecificationEncoder[Attributes: JsonEncoder]: JsonEncoder[TypeModule.Specification[Attributes]] =
    new JsonEncoder[TypeModule.Specification[Attributes]] {
      def unsafeEncode(spec: TypeModule.Specification[Attributes], indent: Option[Int], out: Write): Unit =
        spec match {
          case spec @ TypeModule.Specification.TypeAliasSpecification(_, _) =>
            JsonEncoder[TypeModule.Specification.TypeAliasSpecification[Attributes]].unsafeEncode(spec, indent, out)
          case spec @ TypeModule.Specification.CustomTypeSpecification(_, _) =>
            JsonEncoder[TypeModule.Specification.CustomTypeSpecification[Attributes]].unsafeEncode(spec, indent, out)
          case spec @ TypeModule.Specification.OpaqueTypeSpecification(_) =>
            JsonEncoder[TypeModule.Specification.OpaqueTypeSpecification].unsafeEncode(spec, indent, out)
        }
    }

  implicit def inputParameterEncoder[Attributes](implicit
      attributesEncoder: JsonEncoder[Attributes]
  ): JsonEncoder[ValueModule.InputParameter[Attributes]] =
    Json.encoder.contramap[ValueModule.InputParameter[Attributes]](ip =>
      Json.Arr(toJsonAstOrThrow(ip.name), toJsonAstOrThrow(ip.annotations), toJsonAstOrThrow(ip.tpe))
    )

  implicit def valueDefinitionEncoder[Self, Attributes](implicit
      attributesEncoder: JsonEncoder[Attributes],
      bodyEncoder: JsonEncoder[Self]
  ): JsonEncoder[ValueModule.Definition[Self, Attributes]] = {
    Json.encoder.contramap[ValueModule.Definition[Self, Attributes]] { definition =>
      Json.Obj(
        "inputTypes" -> toJsonAstOrThrow(definition.inputTypes),
        "outputType" -> toJsonAstOrThrow(definition.outputType),
        "body"       -> toJsonAstOrThrow(definition.body)
      )
    }
  }

  implicit def valueSpecificationEncoder[Attributes](implicit
      attributesEncoder: JsonEncoder[Attributes]
  ): JsonEncoder[ValueModule.Specification[Attributes]] = {
    Json.encoder.contramap[ValueModule.Specification[Attributes]] { specification =>
      Json.Obj(
        "inputs"  -> toJsonAstOrThrow(specification.inputs),
        "outputs" -> toJsonAstOrThrow(specification.output)
      )
    }
  }

  implicit def valueEncoder[Attributes](implicit
      attributesEncoder: JsonEncoder[Attributes]
  ): JsonEncoder[Value[Attributes]] = {
    Json.encoder.contramap[Value[Attributes]] { value =>
      value.foldAnnotated[Json] {
        case (ValueCase.UnitCase, attributes) =>
          Json.Arr(Json.Str("unit"), toJsonAstOrThrow(attributes))
        case (ValueCase.RecordCase(fields), attributes) =>
          Json.Arr(Json.Str("record"), toJsonAstOrThrow(attributes), toJsonAstOrThrow(fields))
        case (ValueCase.LiteralCase(literal @ _), attributes) =>
          Json.Arr(Json.Str("literal"), toJsonAstOrThrow(attributes), ???)
        case (ValueCase.ConstructorCase(name), attributes) =>
          Json.Arr(Json.Str("constructor"), toJsonAstOrThrow(attributes), toJsonAstOrThrow(name))
        case (ValueCase.ReferenceCase(name), attributes) =>
          Json.Arr(Json.Str("reference"), toJsonAstOrThrow(attributes), toJsonAstOrThrow(name))
        case (ValueCase.VariableCase(name), attributes) =>
          Json.Arr(Json.Str("variable"), toJsonAstOrThrow(attributes), toJsonAstOrThrow(name))
        case (ValueCase.TupleCase(elements), attributes) =>
          Json.Arr(Json.Str("tuple"), toJsonAstOrThrow(attributes), Json.Arr(elements))
        case (ValueCase.ListCase(elements), attributes) =>
          Json.Arr(Json.Str("list"), toJsonAstOrThrow(attributes), Json.Arr(elements))
        case (ValueCase.FieldCase(target, name), attributes) =>
          Json.Arr(Json.Str("field"), toJsonAstOrThrow(attributes), target, toJsonAstOrThrow(name))
        case (ValueCase.FieldFunctionCase(name), attributes) =>
          Json.Arr(Json.Str("field_function"), toJsonAstOrThrow(attributes), toJsonAstOrThrow(name))
        case (ValueCase.ApplyCase(function, arguments), attributes) =>
          Json.Arr(Json.Str("apply"), toJsonAstOrThrow(attributes), function, Json.Arr(arguments))
        case (ValueCase.LambdaCase(argumentPattern @ _, body), attributes) =>
          Json.Arr(Json.Str("lambda"), toJsonAstOrThrow(attributes), ???, body)
        case (ValueCase.LetDefinitionCase(valueName, valueDefinition, inValue), attributes) =>
          Json.Arr(
            Json.Str("let_definition"),
            toJsonAstOrThrow(attributes),
            toJsonAstOrThrow(valueName),
            toJsonAstOrThrow(valueDefinition.asInstanceOf[ValueModule.Definition[Json, Attributes]]),
            inValue
          )
        case (ValueCase.LetRecursionCase(valueDefinitions, inValue), attributes) =>
          Json.Arr(
            Json.Str("let_recursion"),
            toJsonAstOrThrow(attributes),
            toJsonAstOrThrow(
              valueDefinitions.asInstanceOf[Map[Name, ValueModule.Definition[Json, Attributes]]].toList
            ),
            inValue
          )
        case (ValueCase.DestructureCase(pattern @ _, valueToDestruct, inValue), attributes) =>
          Json.Arr(Json.Str("destructure"), toJsonAstOrThrow(attributes), ???, valueToDestruct, inValue)
        case (ValueCase.IfThenElseCase(condition, thenBranch, elseBranch), attributes) =>
          Json.Arr(Json.Str("if_then_else"), toJsonAstOrThrow(attributes), condition, thenBranch, elseBranch)
        case (ValueCase.PatternMatchCase(branchOutOn, cases @ _), attributes) =>
          Json.Arr(Json.Str("pattern_match"), toJsonAstOrThrow(attributes), branchOutOn, ???)
        case (ValueCase.UpdateRecordCase(valueToUpdate, fieldsToUpdate), attributes) =>
          Json.Arr(
            Json.Str("update_record"),
            toJsonAstOrThrow(attributes),
            valueToUpdate,
            toJsonAstOrThrow(fieldsToUpdate)
          )
        case (ValueCase.NativeApplyCase(nativeFunction @ _, arguments), attributes) =>
          Json.Arr(Json.Str("apply"), toJsonAstOrThrow(attributes), ???, Json.Arr(arguments))
      }
    }
  }

  implicit def ExtensibleRecordTypeJsonEncoder[Attributes: JsonEncoder]
      : JsonEncoder[Type.ExtensibleRecord[Attributes]] =
    JsonEncoder.tuple4[String, Attributes, Name, Chunk[Field[Type[Attributes]]]].contramap {
      case Type.ExtensibleRecord(attributes, name, fields) => ("extensible_record", attributes, name, fields)
    }

  implicit def FunctionTypeJsonEncoder[Attributes: JsonEncoder]: JsonEncoder[Type.Function[Attributes]] =
    JsonEncoder.tuple4[String, Attributes, Chunk[Type[Attributes]], Type[Attributes]].contramap {
      case Type.Function(attributes, paramTypes, returnType) =>
        ("function", attributes, paramTypes, returnType)
    }

  implicit def RecordTypeJsonEncoder[Attributes: JsonEncoder]: JsonEncoder[Type.Record[Attributes]] =
    JsonEncoder.tuple3[String, Attributes, Chunk[Field[Type[Attributes]]]].contramap {
      case Type.Record(attributes, fields) =>
        ("record", attributes, fields)
    }

  implicit def ReferenceTypeJsonEncoder[Attributes: JsonEncoder]: JsonEncoder[Type.Reference[Attributes]] =
    JsonEncoder.tuple4[String, Attributes, FQName, Chunk[Type[Attributes]]].contramap {
      case Type.Reference(attributes, name, typeParams) =>
        ("reference", attributes, name, typeParams)
    }

  implicit def TupleTypeJsonEncoder[Attributes](implicit
      attributesEncoder: JsonEncoder[Attributes],
      typeEncoder: JsonEncoder[Type[Attributes]]
  ): JsonEncoder[Type.Tuple[Attributes]] =
    JsonEncoder.tuple3[String, Attributes, Chunk[Type[Attributes]]].contramap[Type.Tuple[Attributes]] {
      case Type.Tuple(attributes, elements) => ("tuple", attributes, elements)
    }

  implicit def UnitTypeJsonEncoder[Attributes: JsonEncoder]: JsonEncoder[Type.Unit[Attributes]] =
    JsonEncoder.tuple2[String, Attributes].contramap[Type.Unit[Attributes]] { case Type.Unit(attributes) =>
      ("unit", attributes)
    }

  implicit def VariableTypeJsonEncoder[Attributes: JsonEncoder]: JsonEncoder[Type.Variable[Attributes]] =
    JsonEncoder.tuple3[String, Attributes, Name].contramap[Type.Variable[Attributes]] {
      case Type.Variable(attributes, name) => ("variable", attributes, name)
    }

  implicit def typeEncoder[Attributes: JsonEncoder]: JsonEncoder[Type[Attributes]] =
    new JsonEncoder[Type[Attributes]] {
      def unsafeEncode(tpe: Type[Attributes], indent: Option[Int], out: Write): Unit = tpe match {
        case t @ Record(_, _) => JsonEncoder[Type.Record[Attributes]].unsafeEncode(t, indent, out)
        case t @ Reference(_, _, _) =>
          JsonEncoder[Type.Reference[Attributes]].unsafeEncode(t, indent, out)
        case t @ Type.Unit(_) =>
          JsonEncoder[Type.Unit[Attributes]].unsafeEncode(t, indent, out)
        case t @ Type.Function(_, _, _) =>
          JsonEncoder[Type.Function[Attributes]].unsafeEncode(t, indent, out)
        case t @ ExtensibleRecord(_, _, _) =>
          JsonEncoder[Type.ExtensibleRecord[Attributes]].unsafeEncode(t, indent, out)
        case t @ Variable(_, _) => JsonEncoder[Type.Variable[Attributes]].unsafeEncode(t, indent, out)
        case t @ Tuple(_, _)    => JsonEncoder[Type.Tuple[Attributes]].unsafeEncode(t, indent, out)
      }
    }

  implicit def documentedEncoder[A](implicit valueEncoder: JsonEncoder[A]): JsonEncoder[Documented[A]] = {
    Json.encoder.contramap[Documented[A]] { documented =>
      Json.Arr(Json.Str(documented.doc), toJsonAstOrThrow(documented.value))
    }
  }

  implicit def moduleSpecificationEncoder[Attributes](implicit
      attributesEncoder: JsonEncoder[Attributes]
  ): JsonEncoder[ModuleModule.Specification[Attributes]] = {
    Json.encoder.contramap[ModuleModule.Specification[Attributes]] { specification =>
      Json.Obj(
        "types"  -> toJsonAstOrThrow(specification.types.toList),
        "values" -> toJsonAstOrThrow(specification.values.toList)
      )
    }
  }

  implicit def moduleDefinitionEncoder[Attributes](implicit
      attributesEncoder: JsonEncoder[Attributes]
  ): JsonEncoder[ModuleModule.Definition[Attributes]] = {
    Json.encoder.contramap[ModuleModule.Definition[Attributes]] { definition =>
      Json.Obj(
        "types"  -> toJsonAstOrThrow(definition.types.toList),
        "values" -> toJsonAstOrThrow(definition.values.toList)
      )
    }
  }

  implicit def packageSpecificationEncoder[Attributes](implicit
      attributesEncoder: JsonEncoder[Attributes]
  ): JsonEncoder[PackageModule.Specification[Attributes]] = {
    Json.encoder.contramap[PackageModule.Specification[Attributes]] { specification =>
      Json.Obj(
        "modules" -> toJsonAstOrThrow(specification.modules.toList.map { case (name, moduleSpecification) =>
          Json.Obj(
            "name" -> toJsonAstOrThrow(name),
            "spec" -> toJsonAstOrThrow(moduleSpecification)
          )
        })
      )
    }
  }

  implicit def packageDefinitionEncoder[Attributes](implicit
      attributesEncoder: JsonEncoder[Attributes]
  ): JsonEncoder[PackageModule.Definition[Attributes]] = {
    Json.encoder.contramap[PackageModule.Definition[Attributes]] { definition =>
      Json.Obj(
        "modules" -> toJsonAstOrThrow(definition.modules.toList.map { case (name, moduleSpecification) =>
          Json.Obj(
            "name" -> toJsonAstOrThrow(name),
            "def"  -> toJsonAstOrThrow(moduleSpecification)
          )
        })
      )
    }
  }

  private def toJsonAstOrThrow[A](a: A)(implicit encoder: JsonEncoder[A]): Json =
    a.toJsonAST.toOption.get
}

object MorphirJsonEncodingSupportV1 extends MorphirJsonEncodingSupportV1
