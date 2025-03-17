package morphir.ir._type

/** Generated based on IR.Type
*/
object Codec{

  implicit def encodeConstructor[A](
    encodeA: io.circe.Encoder[A]
  ): io.circe.Encoder[morphir.ir.Type.Constructor[A]] =
    ((constructor: (morphir.ir.Name.Name, morphir.ir.Type.ConstructorArgs[A])) =>
      io.circe.Json.arr(
        morphir.ir.name.Codec.encodeName(constructor._1),
        morphir.ir._type.Codec.encodeConstructorArgs(encodeA)(constructor._2)
      ))
  
  implicit def encodeConstructorArgs[A](
    encodeA: io.circe.Encoder[A]
  ): io.circe.Encoder[morphir.ir.Type.ConstructorArgs[A]] =
    morphir.sdk.list.Codec.encodeList(((constructorArgs: (morphir.ir.Name.Name, morphir.ir.Type.Type[A])) =>
      io.circe.Json.arr(
        morphir.ir.name.Codec.encodeName(constructorArgs._1),
        morphir.ir._type.Codec.encodeType(encodeA)(constructorArgs._2)
      )))
  
  implicit def encodeConstructors[A](
    encodeA: io.circe.Encoder[A]
  ): io.circe.Encoder[morphir.ir.Type.Constructors[A]] =
    morphir.sdk.dict.Codec.encodeDict(
      morphir.ir.name.Codec.encodeName,
      morphir.ir._type.Codec.encodeConstructorArgs(encodeA)
    )
  
  implicit def encodeDefinition[A](
    encodeA: io.circe.Encoder[A]
  ): io.circe.Encoder[morphir.ir.Type.Definition[A]] =
    ((definition: morphir.ir.Type.Definition[A]) =>
      definition match {
        case morphir.ir.Type.CustomTypeDefinition(typeParams, ctors) => 
          io.circe.Json.arr(
            io.circe.Json.fromString("""CustomTypeDefinition"""),
            morphir.sdk.list.Codec.encodeList(morphir.ir.name.Codec.encodeName)(typeParams),
            morphir.ir.accesscontrolled.Codec.encodeAccessControlled(morphir.ir._type.Codec.encodeConstructors(encodeA))(ctors)
          )
        case morphir.ir.Type.TypeAliasDefinition(typeParams, typeExp) => 
          io.circe.Json.arr(
            io.circe.Json.fromString("""TypeAliasDefinition"""),
            morphir.sdk.list.Codec.encodeList(morphir.ir.name.Codec.encodeName)(typeParams),
            morphir.ir._type.Codec.encodeType(encodeA)(typeExp)
          )
      })
  
  implicit def encodeDerivedTypeSpecificationDetails[A](
    encodeA: io.circe.Encoder[A]
  ): io.circe.Encoder[morphir.ir.Type.DerivedTypeSpecificationDetails[A]] =
    ((derivedTypeSpecificationDetails: morphir.ir.Type.DerivedTypeSpecificationDetails[A]) =>
      io.circe.Json.obj(
        ("""baseType""", morphir.ir._type.Codec.encodeType(encodeA)(derivedTypeSpecificationDetails.baseType)),
        ("""fromBaseType""", morphir.ir.fqname.Codec.encodeFQName(derivedTypeSpecificationDetails.fromBaseType)),
        ("""toBaseType""", morphir.ir.fqname.Codec.encodeFQName(derivedTypeSpecificationDetails.toBaseType))
      ))
  
  implicit def encodeField[A](
    encodeA: io.circe.Encoder[A]
  ): io.circe.Encoder[morphir.ir.Type.Field[A]] =
    ((field: morphir.ir.Type.Field[A]) =>
      io.circe.Json.obj(
        ("""name""", morphir.ir.name.Codec.encodeName(field.name)),
        ("""tpe""", morphir.ir._type.Codec.encodeType(encodeA)(field.tpe))
      ))
  
  implicit def encodeSpecification[A](
    encodeA: io.circe.Encoder[A]
  ): io.circe.Encoder[morphir.ir.Type.Specification[A]] =
    ((specification: morphir.ir.Type.Specification[A]) =>
      specification match {
        case morphir.ir.Type.CustomTypeSpecification(typeParams, ctors) => 
          io.circe.Json.arr(
            io.circe.Json.fromString("""CustomTypeSpecification"""),
            morphir.sdk.list.Codec.encodeList(morphir.ir.name.Codec.encodeName)(typeParams),
            morphir.ir._type.Codec.encodeConstructors(encodeA)(ctors)
          )
        case morphir.ir.Type.DerivedTypeSpecification(typeParams, details) => 
          io.circe.Json.arr(
            io.circe.Json.fromString("""DerivedTypeSpecification"""),
            morphir.sdk.list.Codec.encodeList(morphir.ir.name.Codec.encodeName)(typeParams),
            morphir.ir._type.Codec.encodeDerivedTypeSpecificationDetails(encodeA)(details)
          )
        case morphir.ir.Type.OpaqueTypeSpecification(typeParams) => 
          io.circe.Json.arr(
            io.circe.Json.fromString("""OpaqueTypeSpecification"""),
            morphir.sdk.list.Codec.encodeList(morphir.ir.name.Codec.encodeName)(typeParams)
          )
        case morphir.ir.Type.TypeAliasSpecification(typeParams, typeExp) => 
          io.circe.Json.arr(
            io.circe.Json.fromString("""TypeAliasSpecification"""),
            morphir.sdk.list.Codec.encodeList(morphir.ir.name.Codec.encodeName)(typeParams),
            morphir.ir._type.Codec.encodeType(encodeA)(typeExp)
          )
      })
  
  implicit def encodeType[A](
    encodeA: io.circe.Encoder[A]
  ): io.circe.Encoder[morphir.ir.Type.Type[A]] =
    ((_type: morphir.ir.Type.Type[A]) =>
      _type match {
        case morphir.ir.Type.ExtensibleRecord(attrs, variableName, fieldTypes) => 
          io.circe.Json.arr(
            io.circe.Json.fromString("""ExtensibleRecord"""),
            encodeA(attrs),
            morphir.ir.name.Codec.encodeName(variableName),
            morphir.sdk.list.Codec.encodeList(morphir.ir._type.Codec.encodeField(encodeA))(fieldTypes)
          )
        case morphir.ir.Type.Function(attrs, argumentType, returnType) => 
          io.circe.Json.arr(
            io.circe.Json.fromString("""Function"""),
            encodeA(attrs),
            morphir.ir._type.Codec.encodeType(encodeA)(argumentType),
            morphir.ir._type.Codec.encodeType(encodeA)(returnType)
          )
        case morphir.ir.Type.Record(attrs, fieldTypes) => 
          io.circe.Json.arr(
            io.circe.Json.fromString("""Record"""),
            encodeA(attrs),
            morphir.sdk.list.Codec.encodeList(morphir.ir._type.Codec.encodeField(encodeA))(fieldTypes)
          )
        case morphir.ir.Type.Reference(attrs, typeName, typeParameters) => 
          io.circe.Json.arr(
            io.circe.Json.fromString("""Reference"""),
            encodeA(attrs),
            morphir.ir.fqname.Codec.encodeFQName(typeName),
            morphir.sdk.list.Codec.encodeList(morphir.ir._type.Codec.encodeType(encodeA))(typeParameters)
          )
        case morphir.ir.Type.Tuple(attrs, elementTypes) => 
          io.circe.Json.arr(
            io.circe.Json.fromString("""Tuple"""),
            encodeA(attrs),
            morphir.sdk.list.Codec.encodeList(morphir.ir._type.Codec.encodeType(encodeA))(elementTypes)
          )
        case morphir.ir.Type.Unit(attrs) => 
          io.circe.Json.arr(
            io.circe.Json.fromString("""Unit"""),
            encodeA(attrs)
          )
        case morphir.ir.Type.Variable(attrs, name) => 
          io.circe.Json.arr(
            io.circe.Json.fromString("""Variable"""),
            encodeA(attrs),
            morphir.ir.name.Codec.encodeName(name)
          )
      })
  
  implicit def decodeConstructor[A](
    decodeA: io.circe.Decoder[A]
  ): io.circe.Decoder[morphir.ir.Type.Constructor[A]] =
    ((c: io.circe.HCursor) =>
      for {
        arg1 <- c.downN(0).as(morphir.ir.name.Codec.decodeName)
        arg2 <- c.downN(1).as(morphir.ir._type.Codec.decodeConstructorArgs(decodeA))
      }  yield (arg1, arg2))
  
  implicit def decodeConstructorArgs[A](
    decodeA: io.circe.Decoder[A]
  ): io.circe.Decoder[morphir.ir.Type.ConstructorArgs[A]] =
    morphir.sdk.list.Codec.decodeList(((c: io.circe.HCursor) =>
      for {
        arg1 <- c.downN(0).as(morphir.ir.name.Codec.decodeName)
        arg2 <- c.downN(1).as(morphir.ir._type.Codec.decodeType(decodeA))
      }  yield (arg1, arg2)))
  
  implicit def decodeConstructors[A](
    decodeA: io.circe.Decoder[A]
  ): io.circe.Decoder[morphir.ir.Type.Constructors[A]] =
    morphir.sdk.dict.Codec.decodeDict(
      morphir.ir.name.Codec.decodeName,
      morphir.ir._type.Codec.decodeConstructorArgs(decodeA)
    )
  
  implicit def decodeDefinition[A](
    decodeA: io.circe.Decoder[A]
  ): io.circe.Decoder[morphir.ir.Type.Definition[A]] =
    ((c: io.circe.HCursor) =>
      c.withFocus(_.withString(((str) =>
        io.circe.Json.arr(io.circe.Json.fromString(str))))).downN(0).as(morphir.sdk.string.Codec.decodeString).flatMap(((tag) =>
        tag match {
          case """CustomTypeDefinition""" => 
            for {
              typeParams <- c.downN(1).as(morphir.sdk.list.Codec.decodeList(morphir.ir.name.Codec.decodeName))
              ctors <- c.downN(2).as(morphir.ir.accesscontrolled.Codec.decodeAccessControlled(morphir.ir._type.Codec.decodeConstructors(decodeA)))
            }  yield morphir.ir.Type.CustomTypeDefinition(
              typeParams,
              ctors
            )
          case """TypeAliasDefinition""" => 
            for {
              typeParams <- c.downN(1).as(morphir.sdk.list.Codec.decodeList(morphir.ir.name.Codec.decodeName))
              typeExp <- c.downN(2).as(morphir.ir._type.Codec.decodeType(decodeA))
            }  yield morphir.ir.Type.TypeAliasDefinition(
              typeParams,
              typeExp
            )
        })))
  
  implicit def decodeDerivedTypeSpecificationDetails[A](
    decodeA: io.circe.Decoder[A]
  ): io.circe.Decoder[morphir.ir.Type.DerivedTypeSpecificationDetails[A]] =
    ((c: io.circe.HCursor) =>
      for {
        baseType_ <- c.downField("""baseType""").as(morphir.ir._type.Codec.decodeType(decodeA))
        fromBaseType_ <- c.downField("""fromBaseType""").as(morphir.ir.fqname.Codec.decodeFQName)
        toBaseType_ <- c.downField("""toBaseType""").as(morphir.ir.fqname.Codec.decodeFQName)
      }  yield morphir.ir.Type.DerivedTypeSpecificationDetails(
        baseType_,
        fromBaseType_,
        toBaseType_
      ))
  
  implicit def decodeField[A](
    decodeA: io.circe.Decoder[A]
  ): io.circe.Decoder[morphir.ir.Type.Field[A]] =
    ((c: io.circe.HCursor) =>
      for {
        name_ <- c.downField("""name""").as(morphir.ir.name.Codec.decodeName)
        tpe_ <- c.downField("""tpe""").as(morphir.ir._type.Codec.decodeType(decodeA))
      }  yield morphir.ir.Type.Field(
        name_,
        tpe_
      ))
  
  implicit def decodeSpecification[A](
    decodeA: io.circe.Decoder[A]
  ): io.circe.Decoder[morphir.ir.Type.Specification[A]] =
    ((c: io.circe.HCursor) =>
      c.withFocus(_.withString(((str) =>
        io.circe.Json.arr(io.circe.Json.fromString(str))))).downN(0).as(morphir.sdk.string.Codec.decodeString).flatMap(((tag) =>
        tag match {
          case """CustomTypeSpecification""" => 
            for {
              typeParams <- c.downN(1).as(morphir.sdk.list.Codec.decodeList(morphir.ir.name.Codec.decodeName))
              ctors <- c.downN(2).as(morphir.ir._type.Codec.decodeConstructors(decodeA))
            }  yield morphir.ir.Type.CustomTypeSpecification(
              typeParams,
              ctors
            )
          case """DerivedTypeSpecification""" => 
            for {
              typeParams <- c.downN(1).as(morphir.sdk.list.Codec.decodeList(morphir.ir.name.Codec.decodeName))
              details <- c.downN(2).as(morphir.ir._type.Codec.decodeDerivedTypeSpecificationDetails(decodeA))
            }  yield morphir.ir.Type.DerivedTypeSpecification(
              typeParams,
              details
            )
          case """OpaqueTypeSpecification""" => 
            for {
              typeParams <- c.downN(1).as(morphir.sdk.list.Codec.decodeList(morphir.ir.name.Codec.decodeName))
            }  yield morphir.ir.Type.OpaqueTypeSpecification(typeParams)
          case """TypeAliasSpecification""" => 
            for {
              typeParams <- c.downN(1).as(morphir.sdk.list.Codec.decodeList(morphir.ir.name.Codec.decodeName))
              typeExp <- c.downN(2).as(morphir.ir._type.Codec.decodeType(decodeA))
            }  yield morphir.ir.Type.TypeAliasSpecification(
              typeParams,
              typeExp
            )
        })))
  
  implicit def decodeType[A](
    decodeA: io.circe.Decoder[A]
  ): io.circe.Decoder[morphir.ir.Type.Type[A]] =
    ((c: io.circe.HCursor) =>
      c.withFocus(_.withString(((str) =>
        io.circe.Json.arr(io.circe.Json.fromString(str))))).downN(0).as(morphir.sdk.string.Codec.decodeString).flatMap(((tag) =>
        tag match {
          case """ExtensibleRecord""" => 
            for {
              attrs <- c.downN(1).as(decodeA)
              variableName <- c.downN(2).as(morphir.ir.name.Codec.decodeName)
              fieldTypes <- c.downN(3).as(morphir.sdk.list.Codec.decodeList(morphir.ir._type.Codec.decodeField(decodeA)))
            }  yield morphir.ir.Type.ExtensibleRecord(
              attrs,
              variableName,
              fieldTypes
            )
          case """Function""" => 
            for {
              attrs <- c.downN(1).as(decodeA)
              argumentType <- c.downN(2).as(morphir.ir._type.Codec.decodeType(decodeA))
              returnType <- c.downN(3).as(morphir.ir._type.Codec.decodeType(decodeA))
            }  yield morphir.ir.Type.Function(
              attrs,
              argumentType,
              returnType
            )
          case """Record""" => 
            for {
              attrs <- c.downN(1).as(decodeA)
              fieldTypes <- c.downN(2).as(morphir.sdk.list.Codec.decodeList(morphir.ir._type.Codec.decodeField(decodeA)))
            }  yield morphir.ir.Type.Record(
              attrs,
              fieldTypes
            )
          case """Reference""" => 
            for {
              attrs <- c.downN(1).as(decodeA)
              typeName <- c.downN(2).as(morphir.ir.fqname.Codec.decodeFQName)
              typeParameters <- c.downN(3).as(morphir.sdk.list.Codec.decodeList(morphir.ir._type.Codec.decodeType(decodeA)))
            }  yield morphir.ir.Type.Reference(
              attrs,
              typeName,
              typeParameters
            )
          case """Tuple""" => 
            for {
              attrs <- c.downN(1).as(decodeA)
              elementTypes <- c.downN(2).as(morphir.sdk.list.Codec.decodeList(morphir.ir._type.Codec.decodeType(decodeA)))
            }  yield morphir.ir.Type.Tuple(
              attrs,
              elementTypes
            )
          case """Unit""" => 
            for {
              attrs <- c.downN(1).as(decodeA)
            }  yield morphir.ir.Type.Unit(attrs)
          case """Variable""" => 
            for {
              attrs <- c.downN(1).as(decodeA)
              name <- c.downN(2).as(morphir.ir.name.Codec.decodeName)
            }  yield morphir.ir.Type.Variable(
              attrs,
              name
            )
        })))

}