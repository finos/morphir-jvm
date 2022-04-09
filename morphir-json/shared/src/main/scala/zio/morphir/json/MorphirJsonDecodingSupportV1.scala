package zio.morphir.json

// import zio._
import zio.json._
import zio.morphir.ir.Value._

trait MorphirJsonDecodingSupportV1 {
  // implicit val unitDecoder: JsonDecoder[Unit] = JsonDecoder.list[String].mapOrFail {
  //   case a if a.isEmpty => Right(())
  //   case a              => Left(s"Expected empty list, got [${a.mkString(", ")}]")
  // }
  // implicit val nameDecoder: JsonDecoder[Name]               = JsonDecoder.list[String].map(Name.fromList)
  // implicit val pathDecoder: JsonDecoder[Path]               = JsonDecoder.list[Name].map(Path.fromList)
  // implicit val modulePathDecoder: JsonDecoder[ModulePath]   = pathDecoder.map(ModulePath(_))
  // implicit val packageNameDecoder: JsonDecoder[PackageName] = pathDecoder.map(PackageName(_))
  // implicit val qNameDecoder: JsonDecoder[QName]             = JsonDecoder.tuple2[Path, Name].map(QName.fromTuple)
  // implicit val fqNameDecoder: JsonDecoder[FQName] = JsonDecoder.tuple3[PackageName, ModulePath, Name].map {
  //   case (packagePath, modulePath, localName) => FQName(packagePath, modulePath, localName)
  // }

  // implicit val moduleNameDecoder: JsonDecoder[ModuleName] =
  //   JsonDecoder.tuple2[Path, Name].map { case (namespace, localName) =>
  //     ModuleName(namespace, localName)
  //   }

  // implicit def literalBoolDecoder: JsonDecoder[Literal.Bool] =
  //   JsonDecoder.tuple2[String, Boolean].mapOrFail {
  //     case ("bool_literal", value) => Right(Literal.Bool(value))
  //     case (other, value)          => Left(s"Expected bool_literal, got $other with value $value")
  //   }

  // implicit def literalCharDecoder: JsonDecoder[Literal.Char] =
  //   JsonDecoder.tuple2[String, Char].mapOrFail {
  //     case ("char_literal", value) => Right(Literal.Char(value))
  //     case (other, value)          => Left(s"Expected char_literal, got $other with value $value")
  //   }

  // implicit def literalFloatDecoder: JsonDecoder[Literal.Float] =
  //   JsonDecoder.tuple2[String, java.math.BigDecimal].mapOrFail {
  //     case ("float_literal", value) => Right(Literal.Float(value))
  //     case (other, value)           => Left(s"Expected float_literal, got $other with value $value")
  //   }

  // implicit def literalStringDecoder: JsonDecoder[Literal.String] =
  //   JsonDecoder.tuple2[String, String].mapOrFail {
  //     case ("string_literal", value) => Right(Literal.String(value))
  //     case (other, value)            => Left(s"Expected string_literal, got $other with value $value")
  //   }

  // implicit def literalWholeNumberDecoder: JsonDecoder[Literal.WholeNumber] =
  //   JsonDecoder.tuple2[String, java.math.BigInteger].mapOrFail {
  //     case ("int_literal", value) => Right(Literal.WholeNumber(value))
  //     case (other, value)         => Left(s"Expected int_literal, got $other with value $value")
  //   }

  // // TODO
  // implicit def literalDecoder[Attributes](implicit
  //     decoder: JsonDecoder[Attributes]
  // ): JsonDecoder[Literal[Attributes]] = ???
  // // literalBoolDecoder.widen orElse
  // //   literalCharDecoder.widen orElse
  // //   literalFloatDecoder.widen orElse
  // //   literalStringDecoder.widen orElse
  // //   literalWholeNumberDecoder.widen

  // implicit def fieldDecoder[A](implicit decoder: JsonDecoder[A]): JsonDecoder[Field[A]] =
  //   JsonDecoder.tuple2[Name, A].map { case (name, fieldType) => Field(name, fieldType) }

  // implicit def documentedDecoder[A](implicit valueDecoder: JsonDecoder[A]): JsonDecoder[Documented[A]] =
  //   JsonDecoder.tuple2[String, A].map { case (doc, value) => Documented(doc, value) }

  // implicit def accessControlledDecoder[A](implicit decoder: JsonDecoder[A]): JsonDecoder[AccessControlled[A]] = {
  //   JsonDecoder.tuple2[String, A].map { case (access, value) =>
  //     AccessControlled(
  //       access match {
  //         case "public"  => Public
  //         case "private" => Private
  //       },
  //       value
  //     )
  //   }
  // }

  // implicit def extensibleRecordTypeDecoder[Attributes](implicit
  //     decoder: JsonDecoder[Attributes]
  // ): JsonDecoder[Type.ExtensibleRecord[Attributes]] =
  //   JsonDecoder.tuple4[String, Attributes, Name, Chunk[Field[Type[Attributes]]]].mapOrFail {
  //     case ("extensible_record", attributes, name, fields) => Right(Type.ExtensibleRecord(attributes, name, fields))
  //     case (other, attributes, name, fields) =>
  //       Left(s"Expected extensible_record, got $other with attributes: $attributes, name: $name and fields: $fields")
  //   }

  // implicit def functionTypeDecoder[Attributes](implicit
  //     decoder: JsonDecoder[Attributes]
  // ): JsonDecoder[Type.Function[Attributes]] =
  //   JsonDecoder.tuple4[String, Attributes, Chunk[Type[Attributes]], Type[Attributes]].mapOrFail {
  //     case ("function", attributes, paramTypes, returnType) => Right(Type.Function(attributes, paramTypes, returnType))
  //     case (other, attributes, paramTypes, returnType) =>
  //       Left(
  //         s"Expected extensible_record, got $other with attributes: $attributes, paramTypes: $paramTypes and returnType: $returnType"
  //       )
  //   }

  // implicit def recordTypeDecoder[Attributes](implicit
  //     decoder: JsonDecoder[Attributes]
  // ): JsonDecoder[Type.Record[Attributes]] =
  //   JsonDecoder.tuple3[String, Attributes, Chunk[Field[Type[Attributes]]]].mapOrFail {
  //     case ("record", attributes, fields) => Right(Type.Record(attributes, fields))
  //     case (other, attributes, fields) =>
  //       Left(s"Expected record, got $other with attributes: $attributes and fields: $fields")
  //   }

  // implicit def referenceTypeDecoder[Attributes](implicit
  //     decoder: JsonDecoder[Attributes]
  // ): JsonDecoder[Type.Reference[Attributes]] =
  //   JsonDecoder.tuple4[String, Attributes, FQName, Chunk[Type[Attributes]]].mapOrFail {
  //     case ("reference", attributes, name, typeParams) => Right(Type.Reference(attributes, name, typeParams))
  //     case (other, attributes, name, typeParams) =>
  //       Left(
  //         s"Expected reference, got $other with attributes: $attributes, name: $name and typeParams: $typeParams"
  //       )
  //   }

  // implicit def tupleTypeDecoder[Attributes](implicit
  //     decoder: JsonDecoder[Attributes]
  // ): JsonDecoder[Type.Tuple[Attributes]] =
  //   JsonDecoder.tuple3[String, Attributes, Chunk[Type[Attributes]]].mapOrFail {
  //     case ("tuple", attributes, elements) => Right(Type.Tuple(attributes, elements))
  //     case (other, attributes, elements) =>
  //       Left(s"Expected tuple, got $other with attributes: $attributes and elements: $elements")
  //   }

  // implicit def unitTypeDecoder[Attributes](implicit
  //     decoder: JsonDecoder[Attributes]
  // ): JsonDecoder[Type.Unit[Attributes]] =
  //   JsonDecoder.tuple2[String, Attributes].mapOrFail {
  //     case ("unit", attributes) => Right(Type.Unit(attributes))
  //     case (other, attributes) =>
  //       Left(s"Expected unit, got $other with attributes: $attributes")
  //   }

  // implicit def variableTypeDecoder[Attributes](implicit
  //     decoder: JsonDecoder[Attributes]
  // ): JsonDecoder[Type.Variable[Attributes]] =
  //   JsonDecoder.tuple3[String, Attributes, Name].mapOrFail {
  //     case ("variable", attributes, name) => Right(Type.Variable(attributes, name))
  //     case (other, attributes, name) =>
  //       Left(s"Expected variable, got $other with attributes: $attributes and name: $name")
  //   }

  // implicit def typeDecoder[Attributes](implicit
  //     decoder: JsonDecoder[Attributes]
  // ): JsonDecoder[Type[Attributes]] =
  //   unitTypeDecoder[Attributes].widen[Type[Attributes]] orElse
  //     variableTypeDecoder[Attributes].widen[Type[Attributes]] orElse
  //     tupleTypeDecoder[Attributes].widen[Type[Attributes]] orElse
  //     recordTypeDecoder[Attributes].widen[Type[Attributes]] orElse
  //     extensibleRecordTypeDecoder[Attributes].widen[Type[Attributes]] orElse
  //     functionTypeDecoder[Attributes].widen[Type[Attributes]] orElse
  //     referenceTypeDecoder[Attributes].widen[Type[Attributes]]

  // implicit def constructorDecoder[Attributes](implicit
  //     decoder: JsonDecoder[Attributes]
  // ): JsonDecoder[Constructors[Attributes]] = {
  //   JsonDecoder.list[(Name, Chunk[(Name, Type[Attributes])])].map {
  //     case Nil          => Constructors(Map.empty)
  //     case constructors => Constructors(constructors.toMap)
  //   }
  // }

  // implicit def typeDefinitionTypeAliasDecoder[Attributes](implicit
  //     decoder: JsonDecoder[Attributes]
  // ): JsonDecoder[zio.morphir.ir.Type.Definition.TypeAlias[Attributes]] =
  //   JsonDecoder.tuple3[String, Chunk[Name], Type[Attributes]].mapOrFail {
  //     case ("type_alias_definition", typeParams, typeExp) =>
  //       Right(zio.morphir.ir.Type.Definition.TypeAlias(typeParams, typeExp))
  //     case (other, typeParams, typeExp) =>
  //       Left(s"Expected type_alias_definition, got $other with typeParams: $typeParams and typeExp: $typeExp")
  //   }

  // implicit def typeDefinitionCustomTypeDecoder[Attributes](implicit
  //     decoder: JsonDecoder[Attributes]
  // ): JsonDecoder[zio.morphir.ir.Type.Definition.CustomType[Attributes]] =
  //   JsonDecoder.tuple3[String, Chunk[Name], AccessControlled[Constructors[Attributes]]].mapOrFail {
  //     case ("custom_type_definition", typeParams, ctors) =>
  //       Right(zio.morphir.ir.Type.Definition.CustomType(typeParams, ctors))
  //     case (other, typeParams, ctors) =>
  //       Left(s"Expected type_alias_definition, got $other with typeParams: $typeParams and ctors: $ctors")
  //   }

  // implicit def typeDefinitionDecoder[Attributes](implicit
  //     decoder: JsonDecoder[Attributes]
  // ): JsonDecoder[zio.morphir.ir.Type.Definition[Attributes]] =
  //   typeDefinitionTypeAliasDecoder[Attributes].widen[zio.morphir.ir.Type.Definition[Attributes]] orElse
  //     typeDefinitionCustomTypeDecoder[Attributes].widen[zio.morphir.ir.Type.Definition[Attributes]]

  // implicit def typeSpecificationTypeAliasDecoder[Attributes](implicit
  //     decoder: JsonDecoder[Attributes]
  // ): JsonDecoder[zio.morphir.ir.Type.Specification.TypeAliasSpecification[Attributes]] =
  //   JsonDecoder.tuple3[String, Chunk[Name], Type[Attributes]].mapOrFail {
  //     case ("type_alias_specification", typeParams, expr) =>
  //       Right(zio.morphir.ir.Type.Specification.TypeAliasSpecification(typeParams, expr))
  //     case (other, typeParams, expr) =>
  //       Left(s"Expected type_alias_specification, got $other with typeParams: $typeParams and expr: $expr")
  //   }

  // implicit def typeSpecificationOpaqueTypeDecoder
  //     : JsonDecoder[zio.morphir.ir.Type.Specification.OpaqueTypeSpecification] =
  //   JsonDecoder.tuple2[String, Chunk[Name]].mapOrFail {
  //     case ("opaque_type_specification", typeParams) =>
  //       Right(zio.morphir.ir.Type.Specification.OpaqueTypeSpecification(typeParams))
  //     case (other, typeParams) =>
  //       Left(s"Expected opaque_type_specification, got $other with typeParams: $typeParams")
  //   }

  // implicit def typeSpecificationCustomTypeDecoder[Attributes](implicit
  //     decoder: JsonDecoder[Attributes]
  // ): JsonDecoder[zio.morphir.ir.Type.Specification.CustomTypeSpecification[Attributes]] =
  //   JsonDecoder.tuple3[String, Chunk[Name], Constructors[Attributes]].mapOrFail {
  //     case ("custom_type_specification", typeParams, ctors) =>
  //       Right(zio.morphir.ir.Type.Specification.CustomTypeSpecification(typeParams, ctors))
  //     case (other, typeParams, ctors) =>
  //       Left(s"Expected custom_type_specification, got $other with typeParams: $typeParams and ctors: $ctors")
  //   }

  // implicit def typeSpecificationDecoder[Attributes](implicit
  //     decoder: JsonDecoder[Attributes]
  // ): JsonDecoder[zio.morphir.ir.Type.Specification[Attributes]] =
  //   typeSpecificationTypeAliasDecoder[Attributes].widen[zio.morphir.ir.Type.Specification[Attributes]] orElse
  //     typeSpecificationCustomTypeDecoder[Attributes].widen[zio.morphir.ir.Type.Specification[Attributes]] orElse
  //     typeSpecificationOpaqueTypeDecoder.widen[zio.morphir.ir.Type.Specification[Attributes]]

  // implicit def inputParameterDecoder[Attributes](implicit
  //     decoder: JsonDecoder[Attributes]
  // ): JsonDecoder[ValueModule.InputParameter[Attributes]] =
  //   JsonDecoder.tuple3[Name, Attributes, Type[Attributes]].map { case (name, attributes, tpe) =>
  //     ValueModule.InputParameter(name, tpe, attributes)
  //   }

  // implicit def valueDefinitionDecoder[Self, Attributes](implicit
  //     decoder: JsonDecoder[Attributes],
  //     bodyDecoder: JsonDecoder[Self]
  // ): JsonDecoder[ValueModule.Definition[Self, Attributes]] = {
  //   lazy val dec: JsonDecoder[ValueModule.Definition[Self, Attributes]] = DeriveJsonDecoder.gen
  //   dec
  // }

  // implicit def valueSpecificationDecoder[Attributes](implicit
  //     decoder: JsonDecoder[Attributes]
  // ): JsonDecoder[ValueModule.Specification[Attributes]] = {
  //   final case class Spec[Attributes](inputs: Chunk[(Name, Type[Attributes])], outputs: Type[Attributes])
  //   lazy val dec: JsonDecoder[Spec[Attributes]] = DeriveJsonDecoder.gen
  //   dec.map { spec => ValueModule.Specification(spec.inputs, spec.outputs) }
  // }

  // implicit def patternAsPatternDecoder[Attributes](implicit
  //     decoder: JsonDecoder[Attributes]
  // ): JsonDecoder[Pattern.AsPattern[Attributes]] =
  //   JsonDecoder.tuple4[String, Attributes, Pattern[Attributes], Name].mapOrFail {
  //     case ("as_pattern", attributes, pattern, name) => Right(Pattern.AsPattern(pattern, name, attributes))
  //     case (other, attributes, pattern, name) =>
  //       Left(
  //         s"Expected as_pattern, got $other with attributes: $attributes, pattern: $pattern and name: $name"
  //       )
  //   }

  // implicit def patternConstructorPatternDecoder[Attributes](implicit
  //     decoder: JsonDecoder[Attributes]
  // ): JsonDecoder[Pattern.ConstructorPattern[Attributes]] =
  //   JsonDecoder.tuple4[String, Attributes, FQName, Chunk[Pattern[Attributes]]].mapOrFail {
  //     case ("constructor_pattern", attributes, constructorName, argumentPatterns) =>
  //       Right(Pattern.ConstructorPattern(constructorName, argumentPatterns, attributes))
  //     case (other, attributes, constructorName, argumentPatterns) =>
  //       Left(
  //         s"Expected constructor_pattern, got $other with attributes: $attributes, constructorName: $constructorName and argumentPatterns: $argumentPatterns"
  //       )
  //   }

  // implicit def patternEmptyListPatternDecoder[Attributes](implicit
  //     decoder: JsonDecoder[Attributes]
  // ): JsonDecoder[Pattern.EmptyListPattern[Attributes]] =
  //   JsonDecoder.tuple2[String, Attributes].mapOrFail {
  //     case ("empty_list_pattern", attributes) =>
  //       Right(Pattern.EmptyListPattern[Attributes](attributes))
  //     case (other, attributes) =>
  //       Left(s"Expected empty_list_pattern, got $other with attributes: $attributes")
  //   }

  // implicit def patternHeadTailPatternDecoder[Attributes](implicit
  //     decoder: JsonDecoder[Attributes]
  // ): JsonDecoder[Pattern.HeadTailPattern[Attributes]] =
  //   JsonDecoder.tuple4[String, Attributes, Pattern[Attributes], Pattern[Attributes]].mapOrFail {
  //     case ("head_tail_pattern", attributes, headPattern, tailPattern) =>
  //       Right(Pattern.HeadTailPattern(headPattern, tailPattern, attributes))
  //     case (other, attributes, headPattern, tailPattern) =>
  //       Left(
  //         s"Expected head_tail_pattern, got $other with attributes: $attributes, headPattern: $headPattern and tailPattern: $tailPattern"
  //       )
  //   }

  // implicit def patternLiteralPatternDecoder[A, Attributes](implicit
  //     decoder: JsonDecoder[Attributes],
  //     literalDecoder: JsonDecoder[Literal[A]] // TODO is this correct?
  // ): JsonDecoder[Pattern.LiteralPattern[A, Attributes]] =
  //   JsonDecoder.tuple3[String, Attributes, Literal[A]].mapOrFail {
  //     case ("literal_pattern", attributes, literal) =>
  //       Right(Pattern.LiteralPattern(literal, attributes))
  //     case (other, attributes, literal) =>
  //       Left(s"Expected literal_pattern, got $other with attributes: $attributes and literal: $literal")
  //   }

  // implicit def patternTuplePatternDecoder[Attributes](implicit
  //     decoder: JsonDecoder[Attributes]
  // ): JsonDecoder[Pattern.TuplePattern[Attributes]] =
  //   JsonDecoder.tuple3[String, Attributes, Chunk[Pattern[Attributes]]].mapOrFail {
  //     case ("tuple_pattern", attributes, elementPatterns) =>
  //       Right(Pattern.TuplePattern(elementPatterns, attributes))
  //     case (other, attributes, elementPatterns) =>
  //       Left(s"Expected tuple_pattern, got $other with attributes: $attributes and elementPatterns: $elementPatterns")
  //   }

  // implicit def patternUnitPatternDecoder[Attributes](implicit
  //     decoder: JsonDecoder[Attributes]
  // ): JsonDecoder[Pattern.UnitPattern[Attributes]] =
  //   JsonDecoder.tuple2[String, Attributes].mapOrFail {
  //     case ("unit_pattern", attributes) =>
  //       Right(Pattern.UnitPattern[Attributes](attributes))
  //     case (other, attributes) =>
  //       Left(s"Expected unit_pattern, got $other with attributes: $attributes")
  //   }
  // implicit def patternWildcardPatternDecoder[Attributes](implicit
  //     decoder: JsonDecoder[Attributes]
  // ): JsonDecoder[Pattern.WildcardPattern[Attributes]] =
  //   JsonDecoder.tuple2[String, Attributes].mapOrFail {
  //     case ("wildcard_pattern", attributes) =>
  //       Right(Pattern.WildcardPattern[Attributes](attributes))
  //     case (other, attributes) =>
  //       Left(s"Expected wildcard_pattern, got $other with attributes: $attributes")
  //   }

  // implicit def patternDecoder[Attributes](implicit decoder: JsonDecoder[Attributes]): JsonDecoder[Pattern[Attributes]] =
  //   patternEmptyListPatternDecoder[Attributes].widen[Pattern[Attributes]] orElse
  //     patternWildcardPatternDecoder[Attributes].widen[Pattern[Attributes]] orElse
  //     patternUnitPatternDecoder[Attributes].widen[Pattern[Attributes]] orElse
  //     // patternLiteralPatternDecoder[???,Attributes].widen[Pattern[Attributes]] orElse
  //     patternTuplePatternDecoder[Attributes].widen[Pattern[Attributes]] orElse
  //     patternHeadTailPatternDecoder[Attributes].widen[Pattern[Attributes]] orElse
  //     patternConstructorPatternDecoder[Attributes].widen[Pattern[Attributes]] orElse
  //     patternAsPatternDecoder[Attributes].widen[Pattern[Attributes]]

  // implicit def moduleSpecificationDecoder[Attributes](implicit
  //     decoder: JsonDecoder[Attributes]
  // ): JsonDecoder[ModuleModule.Specification[Attributes]] = {
  //   final case class Spec[Attributes](
  //       types: List[(Name, Documented[zio.morphir.ir.Type.Specification[Attributes]])],
  //       values: List[(Name, Documented[ValueModule.Specification[Attributes]])]
  //   )
  //   lazy val dec: JsonDecoder[Spec[Attributes]] = DeriveJsonDecoder.gen
  //   dec.map { spec => ModuleModule.Specification(spec.types.toMap, spec.values.toMap) }
  // }

  // implicit def moduleDefinitionDecoder[Attributes](implicit
  //     decoder: JsonDecoder[Attributes]
  // ): JsonDecoder[ModuleModule.Definition[Attributes]] = {
  //   final case class Def[Attributes](
  //       types: List[(Name, AccessControlled[Documented[zio.morphir.ir.Type.Definition[Attributes]]])],
  //       values: List[(Name, AccessControlled[Documented[ValueModule.ValueDefinition[Attributes]]])]
  //   )
  //   lazy val dec1: JsonDecoder[Def[Attributes]] = DeriveJsonDecoder.gen
  //   dec1.map { d => ModuleModule.Definition(d.types.toMap, d.values.toMap) }
  // }

  // implicit def packageModuleSpecificationDecoder[Attributes](implicit
  //     decoder: JsonDecoder[Attributes]
  // ): JsonDecoder[PackageModule.Specification[Attributes]] = {
  //   final case class Module[Attributes](name: ModuleName, spec: ModuleModule.Specification[Attributes])
  //   final case class Spec[Attributes](modules: List[Module[Attributes]])

  //   implicit val modDec: JsonDecoder[Module[Attributes]] = DeriveJsonDecoder.gen
  //   lazy val _                                           = modDec // This is to suppress unused local val warning
  //   lazy val dec: JsonDecoder[Spec[Attributes]]          = DeriveJsonDecoder.gen
  //   dec.map { s => PackageModule.Specification(s.modules.map { m => m.name -> m.spec }.toMap) }
  // }

  // implicit def packageModuleDefinitionDecoder[Attributes](implicit
  //     decoder: JsonDecoder[Attributes]
  // ): JsonDecoder[PackageModule.Definition[Attributes]] = {
  //   final case class Module[Attributes](name: ModuleName, `def`: AccessControlled[ModuleDefinition[Attributes]])
  //   final case class Spec[Attributes](modules: List[Module[Attributes]])

  //   implicit val modDec: JsonDecoder[Module[Attributes]] = DeriveJsonDecoder.gen
  //   lazy val _                                           = modDec // This is to suppress unused local val warning
  //   lazy val dec: JsonDecoder[Spec[Attributes]]          = DeriveJsonDecoder.gen
  //   dec.map { d => PackageModule.Definition(d.modules.map { m => m.name -> m.`def` }.toMap) }
  // }

  // TODO
  implicit def valueDecoder[TA, VA](implicit
      taDecoder: JsonDecoder[TA],
      vaDecoder: JsonDecoder[VA]
  ): JsonDecoder[Value[TA, VA]] = ???
}

object MorphirJsonDecodingSupportV1 extends MorphirJsonDecodingSupportV1
