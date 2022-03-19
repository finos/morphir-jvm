package zio.morphir.json

import zio._
import zio.json._
import zio.json.ast.Json
import zio.morphir.ir._
import zio.morphir.ir.AccessControlled.Access._
import zio.morphir.ir.Literal
import zio.morphir.ir.ValueModule.{Value, ValueCase}
import zio.morphir.ir.TypeModule._
import zio.morphir.ir.TypeModule.Definition._
import zio.morphir.ir.TypeModule.Specification._

object Encoders {
  trait MorphirJsonCodecV1 {
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
      Json.encoder.contramap[Field[A]](field => Json.Arr(toJsonAstOrThrow(field.name), toJsonAstOrThrow(field.tpe)))

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

    implicit def literalEncoder[A]: JsonEncoder[Literal[A]] = Json.encoder.contramap[Literal[A]] { ??? }

    implicit def patternEncoder[Annotations](implicit
        annotationsEncoder: JsonEncoder[Annotations]
    ): JsonEncoder[Pattern[Annotations]] = Json.encoder.contramap[Pattern[Annotations]] { pattern =>
      pattern match {
        case Pattern.AsPattern(pattern @ _, name, annotations) =>
          Json.Arr(
            Json.Str("as_pattern"),
            toJsonAstOrThrow(annotations),
            ???, // toJsonAstOrThrow(pattern),
            toJsonAstOrThrow(name)
          )
        case Pattern.ConstructorPattern(constructorName, argumentPatterns @ _, annotations) =>
          Json.Arr(
            Json.Str("constructor_pattern"),
            toJsonAstOrThrow(annotations),
            toJsonAstOrThrow(constructorName),
            ??? // Json.Arr(argumentPatterns.map(toJsonAstOrThrow(_)))
          )
        case Pattern.EmptyListPattern(annotations) =>
          Json.Arr(Json.Str("empty_list_pattern"), toJsonAstOrThrow(annotations))
        case Pattern.HeadTailPattern(headPattern @ _, tailPattern @ _, annotations) =>
          Json.Arr(
            Json.Str("head_tail_pattern"),
            toJsonAstOrThrow(annotations),
            ???, // toJsonAstOrThrow(headPattern),
            ???  // toJsonAstOrThrow(tailPattern)
          )
        case Pattern.LiteralPattern(literal, annotations) =>
          Json.Arr(Json.Str("literal_pattern"), toJsonAstOrThrow(annotations), toJsonAstOrThrow(literal))
        case Pattern.TuplePattern(patterns @ _, annotations) =>
          Json.Arr(
            Json.Str("tuple_pattern"),
            toJsonAstOrThrow(annotations),
            ??? // Json.Arr(patterns.map(toJsonAstOrThrow(_)))
          )
        case Pattern.UnitPattern(annotations) =>
          Json.Arr(Json.Str("unit_pattern"), toJsonAstOrThrow(annotations))
        case Pattern.WildcardPattern(annotations) =>
          Json.Arr(Json.Str("wildcard_pattern"), toJsonAstOrThrow(annotations))
      }
    }

    implicit def constructorsEncoder[Annotations](implicit
        annotationsEncoder: JsonEncoder[Annotations]
    ): JsonEncoder[Constructors[Annotations]] = {
      Json.encoder.contramap[Constructors[Annotations]] { ctors =>
        Json.Arr(
          (
            toJsonAstOrThrow(
              ctors.items.toList.map { case (ctorName: Name, ctorArgs: Chunk[(Name, Type[Annotations])]) =>
                (
                  toJsonAstOrThrow(ctorName),
                  toJsonAstOrThrow(
                    ctorArgs.map { case (argName: Name, argType: Type[Annotations]) =>
                      Json.Arr(toJsonAstOrThrow(argName), toJsonAstOrThrow(argType))
                    }
                  )
                )
              }
            )
          )
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

    implicit def typeDefinitionEncoder[Annotations](implicit
        annotationsEncoder: JsonEncoder[Annotations]
    ): JsonEncoder[TypeModule.Definition[Annotations]] = {
      Json.encoder.contramap[TypeModule.Definition[Annotations]] { definition =>
        definition match {
          case TypeAlias(typeParams, typeExp) =>
            Json.Arr(Json.Str("type_alias_definition"), toJsonAstOrThrow(typeParams), toJsonAstOrThrow(typeExp))
          case CustomType(typeParams, ctors) => {
            Json.Arr(Json.Str("custom_type_definition"), toJsonAstOrThrow(typeParams), toJsonAstOrThrow(ctors))
          }
        }
      }
    }

    implicit def typeSpecificationEncoder[Annotations](implicit
        annotationsEncoder: JsonEncoder[Annotations]
    ): JsonEncoder[TypeModule.Specification[Annotations]] = {
      Json.encoder.contramap[TypeModule.Specification[Annotations]] { specification =>
        specification match {
          case TypeAliasSpecification(typeParams, expr, _) => {
            Json.Arr(
              Json.Str("type_alias_specification"),
              toJsonAstOrThrow(typeParams),
              toJsonAstOrThrow(expr)
            )
          }
          case CustomTypeSpecification(typeParams, ctors, _) => {
            Json.Arr(
              Json.Str("custom_type_specification"),
              toJsonAstOrThrow(typeParams),
              toJsonAstOrThrow(ctors)
            )
          }
          case OpaqueTypeSpecification(typeParams, _) => {
            Json.Arr(
              Json.Str("opaque_type_specification"),
              toJsonAstOrThrow(typeParams)
            )
          }
        }
      }
    }

    implicit def inputParameterEncoder[Annotations](implicit
        annotationsEncoder: JsonEncoder[Annotations]
    ): JsonEncoder[ValueModule.InputParameter[Annotations]] =
      Json.encoder.contramap[ValueModule.InputParameter[Annotations]](ip =>
        Json.Arr(toJsonAstOrThrow(ip.name), toJsonAstOrThrow(ip.annotations), toJsonAstOrThrow(ip.tpe))
      )

    implicit def valueDefinitionEncoder[Self, Annotations](implicit
        annotationsEncoder: JsonEncoder[Annotations],
        bodyEncoder: JsonEncoder[Self]
    ): JsonEncoder[ValueModule.Definition[Self, Annotations]] = {
      Json.encoder.contramap[ValueModule.Definition[Self, Annotations]] { definition =>
        Json.Obj(
          "inputTypes" -> toJsonAstOrThrow(definition.inputTypes),
          "outputType" -> toJsonAstOrThrow(definition.outputType),
          "body"       -> toJsonAstOrThrow(definition.body)
        )
      }
    }

    implicit def valueSpecificationEncoder[Annotations](implicit
        annotationsEncoder: JsonEncoder[Annotations]
    ): JsonEncoder[ValueModule.Specification[Annotations]] = {
      Json.encoder.contramap[ValueModule.Specification[Annotations]] { specification =>
        Json.Obj(
          "inputs"  -> toJsonAstOrThrow(specification.inputs),
          "outputs" -> toJsonAstOrThrow(specification.output)
        )
      }
    }

    implicit def valueEncoder[Annotations](implicit
        annotationsEncoder: JsonEncoder[Annotations]
    ): JsonEncoder[Value[Annotations]] = {
      Json.encoder.contramap[Value[Annotations]] { value =>
        value.foldAnnotated[Json] {
          case (ValueCase.UnitCase, annotations) =>
            Json.Arr(Json.Str("unit"), toJsonAstOrThrow(annotations))
          case (ValueCase.RecordCase(fields), annotations) =>
            Json.Arr(Json.Str("record"), toJsonAstOrThrow(annotations), toJsonAstOrThrow(fields))
          case (ValueCase.LiteralCase(literal), annotations) =>
            Json.Arr(Json.Str("literal"), toJsonAstOrThrow(annotations), toJsonAstOrThrow(literal))
          case (ValueCase.ConstructorCase(name), annotations) =>
            Json.Arr(Json.Str("constructor"), toJsonAstOrThrow(annotations), toJsonAstOrThrow(name))
          case (ValueCase.ReferenceCase(name), annotations) =>
            Json.Arr(Json.Str("reference"), toJsonAstOrThrow(annotations), toJsonAstOrThrow(name))
          case (ValueCase.VariableCase(name), annotations) =>
            Json.Arr(Json.Str("variable"), toJsonAstOrThrow(annotations), toJsonAstOrThrow(name))
          case (ValueCase.TupleCase(elements), annotations) =>
            Json.Arr(Json.Str("tuple"), toJsonAstOrThrow(annotations), Json.Arr(elements))
          case (ValueCase.ListCase(elements), annotations) =>
            Json.Arr(Json.Str("list"), toJsonAstOrThrow(annotations), Json.Arr(elements))
          case (ValueCase.FieldCase(target, name), annotations) =>
            Json.Arr(Json.Str("field"), toJsonAstOrThrow(annotations), target, toJsonAstOrThrow(name))
          case (ValueCase.FieldFunctionCase(name), annotations) =>
            Json.Arr(Json.Str("field_function"), toJsonAstOrThrow(annotations), toJsonAstOrThrow(name))
          case (ValueCase.ApplyCase(function, arguments), annotations) =>
            Json.Arr(Json.Str("apply"), toJsonAstOrThrow(annotations), function, Json.Arr(arguments))
          case (ValueCase.LambdaCase(argumentPattern @ _, body), annotations) =>
            Json.Arr(Json.Str("lambda"), toJsonAstOrThrow(annotations), ???, body)
          case (ValueCase.LetDefinitionCase(valueName, valueDefinition @ _, inValue), annotations) =>
            Json.Arr(
              Json.Str("let_definition"),
              toJsonAstOrThrow(annotations),
              toJsonAstOrThrow(valueName),
              ???,
              inValue
            )
          case (ValueCase.LetRecursionCase(valueDefinitions @ _, inValue), annotations) =>
            Json.Arr(Json.Str("let_recursion"), toJsonAstOrThrow(annotations), ???, inValue)
          case (ValueCase.DestructureCase(pattern @ _, valueToDestruct, inValue), annotations) =>
            Json.Arr(Json.Str("destructure"), toJsonAstOrThrow(annotations), ???, valueToDestruct, inValue)
          case (ValueCase.IfThenElseCase(condition, thenBranch, elseBranch), annotations) =>
            Json.Arr(Json.Str("if_then_else"), toJsonAstOrThrow(annotations), condition, thenBranch, elseBranch)
          case (ValueCase.PatternMatchCase(branchOutOn, cases @ _), annotations) =>
            Json.Arr(Json.Str("pattern_match"), toJsonAstOrThrow(annotations), branchOutOn, ???)
          case (ValueCase.UpdateRecordCase(valueToUpdate, fieldsToUpdate), annotations) =>
            Json.Arr(
              Json.Str("update_record"),
              toJsonAstOrThrow(annotations),
              valueToUpdate,
              toJsonAstOrThrow(fieldsToUpdate)
            )
          case (ValueCase.NativeApplyCase(nativeFunction @ _, arguments), annotations) =>
            Json.Arr(Json.Str("apply"), toJsonAstOrThrow(annotations), ???, Json.Arr(arguments))
        }
      }
    }

    // sealed trait Annotation
    // }
    // object Annotation {
    //   XYZ extends Annotation
    // }
    // }

    // A type level map where all entries in the map are guaranteed to have an
    // instance of the type class TypeClass

    // Map[TypeTag, (Implementation, TypeClass)]
    sealed trait ZEnvironmentSubset[+R, TypeClass[_]] {
      def unsafeMap: Map[Tag[_], (Any, Any)]
      def get[A >: R]: A
      def instance[A >: R]: TypeClass[A]
    }

    // the typeclass with no capabilities that exists for every data type
    // trait AnyF[_]

    // type ZEnvironmentUnconstrained[+R] = ZEnvironmentSubset[R, AnyF]

    // def encodeEnvironment[R](environment: ZEnvironmentSubset[R, JsonEncoder]): Json = {
    //   environment.unsafeMap.map { case (tag, (value, encoder)) =>
    //     ??? // Json.Arr(tag.toJsonAST.right.get, encoder.encode(value))
    //   }     // Iterable[JSON] --> Json.Arr
    //   ???
    // }

    implicit def typeEncoder[Annotations](implicit
        annotationsEncoder: JsonEncoder[Annotations]
    ): JsonEncoder[Type[Annotations]] =
      Json.encoder.contramap[Type[Annotations]] { tpe =>
        tpe.foldAttributed[Json] {
          case (TypeCase.ExtensibleRecordCase(name, fields), annotations) =>
            Json.Arr(
              Json.Str("extensible_record"),
              toJsonAstOrThrow(annotations),
              toJsonAstOrThrow(name),
              Json.Arr(fields.map(toJsonAstOrThrow(_)))
            )
          case (TypeCase.FunctionCase(paramTypes, returnType), annotations) =>
            Json.Arr(Json.Str("function"), toJsonAstOrThrow(annotations), Json.Arr(paramTypes), returnType)
          case (TypeCase.RecordCase(fields), annotations) =>
            Json.Arr(Json.Str("record"), toJsonAstOrThrow(annotations), Json.Arr(fields.map(toJsonAstOrThrow(_))))
          case (TypeCase.ReferenceCase(typeName, typeParameters), annotations) =>
            Json.Arr(
              Json.Str("reference"),
              toJsonAstOrThrow(annotations),
              toJsonAstOrThrow(typeName),
              Json.Arr(typeParameters)
            )
          case (TypeCase.TupleCase(items), annotations) =>
            Json.Arr(Json.Str("tuple"), toJsonAstOrThrow(annotations), Json.Arr(items))
          case (TypeCase.VariableCase(name), annotations) =>
            Json.Arr(Json.Str("variable"), toJsonAstOrThrow(annotations), toJsonAstOrThrow(name))
          case (TypeCase.UnitCase, annotations) => Json.Arr(Json.Str("unit"), toJsonAstOrThrow(annotations))
        }
      }

    implicit def documentedEncoder[A](implicit valueEncoder: JsonEncoder[A]): JsonEncoder[Documented[A]] = {
      Json.encoder.contramap[Documented[A]] { documented =>
        Json.Arr(Json.Str(documented.doc), toJsonAstOrThrow(documented.value))
      }
    }

    implicit def moduleSpecificationEncoder[Annotations](implicit
        annotationsEncoder: JsonEncoder[Annotations]
    ): JsonEncoder[ModuleModule.Specification[Annotations]] = {
      Json.encoder.contramap[ModuleModule.Specification[Annotations]] { specification =>
        Json.Obj(
          "types"  -> toJsonAstOrThrow(specification.types.toList),
          "values" -> toJsonAstOrThrow(specification.values.toList)
        )
      }
    }

    implicit def moduleDefinitionEncoder[Annotations](implicit
        annotationsEncoder: JsonEncoder[Annotations]
    ): JsonEncoder[ModuleModule.Definition[Annotations]] = {
      Json.encoder.contramap[ModuleModule.Definition[Annotations]] { definition =>
        Json.Obj(
          "types"  -> toJsonAstOrThrow(definition.types.toList),
          "values" -> toJsonAstOrThrow(definition.values.toList)
        )
      }
    }

    implicit def packageSpecificationEncoder[Annotations](implicit
        annotationsEncoder: JsonEncoder[Annotations]
    ): JsonEncoder[PackageModule.Specification[Annotations]] = {
      Json.encoder.contramap[PackageModule.Specification[Annotations]] { specification =>
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

    implicit def packageDefinitionEncoder[Annotations](implicit
        annotationsEncoder: JsonEncoder[Annotations]
    ): JsonEncoder[PackageModule.Definition[Annotations]] = {
      Json.encoder.contramap[PackageModule.Definition[Annotations]] { definition =>
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

  object MorphirJsonCodecV1 extends MorphirJsonCodecV1
}
