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
        case morphir.ir.Type.CustomTypeDefinition(arg1, arg2) => 
          io.circe.Json.arr(
            io.circe.Json.fromString("""CustomTypeDefinition"""),
            morphir.sdk.list.Codec.encodeList(morphir.ir.name.Codec.encodeName)(arg1),
            morphir.ir.accesscontrolled.Codec.encodeAccessControlled(morphir.ir._type.Codec.encodeConstructors(encodeA))(arg2)
          )
        case morphir.ir.Type.TypeAliasDefinition(arg1, arg2) => 
          io.circe.Json.arr(
            io.circe.Json.fromString("""TypeAliasDefinition"""),
            morphir.sdk.list.Codec.encodeList(morphir.ir.name.Codec.encodeName)(arg1),
            morphir.ir._type.Codec.encodeType(encodeA)(arg2)
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
        case morphir.ir.Type.CustomTypeSpecification(arg1, arg2) => 
          io.circe.Json.arr(
            io.circe.Json.fromString("""CustomTypeSpecification"""),
            morphir.sdk.list.Codec.encodeList(morphir.ir.name.Codec.encodeName)(arg1),
            morphir.ir._type.Codec.encodeConstructors(encodeA)(arg2)
          )
        case morphir.ir.Type.DerivedTypeSpecification(arg1, arg2) => 
          io.circe.Json.arr(
            io.circe.Json.fromString("""DerivedTypeSpecification"""),
            morphir.sdk.list.Codec.encodeList(morphir.ir.name.Codec.encodeName)(arg1),
            morphir.ir._type.Codec.encodeDerivedTypeSpecificationDetails(encodeA)(arg2)
          )
        case morphir.ir.Type.OpaqueTypeSpecification(arg1) => 
          io.circe.Json.arr(
            io.circe.Json.fromString("""OpaqueTypeSpecification"""),
            morphir.sdk.list.Codec.encodeList(morphir.ir.name.Codec.encodeName)(arg1)
          )
        case morphir.ir.Type.TypeAliasSpecification(arg1, arg2) => 
          io.circe.Json.arr(
            io.circe.Json.fromString("""TypeAliasSpecification"""),
            morphir.sdk.list.Codec.encodeList(morphir.ir.name.Codec.encodeName)(arg1),
            morphir.ir._type.Codec.encodeType(encodeA)(arg2)
          )
      })
  
  implicit def encodeType[A](
    encodeA: io.circe.Encoder[A]
  ): io.circe.Encoder[morphir.ir.Type.Type[A]] =
    ((_type: morphir.ir.Type.Type[A]) =>
      _type match {
        case morphir.ir.Type.ExtensibleRecord(arg1, arg2, arg3) => 
          io.circe.Json.arr(
            io.circe.Json.fromString("""ExtensibleRecord"""),
            encodeA(arg1),
            morphir.ir.name.Codec.encodeName(arg2),
            morphir.sdk.list.Codec.encodeList(morphir.ir._type.Codec.encodeField(encodeA))(arg3)
          )
        case morphir.ir.Type.Function(arg1, arg2, arg3) => 
          io.circe.Json.arr(
            io.circe.Json.fromString("""Function"""),
            encodeA(arg1),
            morphir.ir._type.Codec.encodeType(encodeA)(arg2),
            morphir.ir._type.Codec.encodeType(encodeA)(arg3)
          )
        case morphir.ir.Type.Record(arg1, arg2) => 
          io.circe.Json.arr(
            io.circe.Json.fromString("""Record"""),
            encodeA(arg1),
            morphir.sdk.list.Codec.encodeList(morphir.ir._type.Codec.encodeField(encodeA))(arg2)
          )
        case morphir.ir.Type.Reference(arg1, arg2, arg3) => 
          io.circe.Json.arr(
            io.circe.Json.fromString("""Reference"""),
            encodeA(arg1),
            morphir.ir.fqname.Codec.encodeFQName(arg2),
            morphir.sdk.list.Codec.encodeList(morphir.ir._type.Codec.encodeType(encodeA))(arg3)
          )
        case morphir.ir.Type.Tuple(arg1, arg2) => 
          io.circe.Json.arr(
            io.circe.Json.fromString("""Tuple"""),
            encodeA(arg1),
            morphir.sdk.list.Codec.encodeList(morphir.ir._type.Codec.encodeType(encodeA))(arg2)
          )
        case morphir.ir.Type.Unit(arg1) => 
          io.circe.Json.arr(
            io.circe.Json.fromString("""Unit"""),
            encodeA(arg1)
          )
        case morphir.ir.Type.Variable(arg1, arg2) => 
          io.circe.Json.arr(
            io.circe.Json.fromString("""Variable"""),
            encodeA(arg1),
            morphir.ir.name.Codec.encodeName(arg2)
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
              arg1 <- c.downN(1).as(morphir.sdk.list.Codec.decodeList(morphir.ir.name.Codec.decodeName))
              arg2 <- c.downN(2).as(morphir.ir.accesscontrolled.Codec.decodeAccessControlled(morphir.ir._type.Codec.decodeConstructors(decodeA)))
            }  yield morphir.ir.Type.CustomTypeDefinition(
              arg1,
              arg2
            )
          case """TypeAliasDefinition""" => 
            for {
              arg1 <- c.downN(1).as(morphir.sdk.list.Codec.decodeList(morphir.ir.name.Codec.decodeName))
              arg2 <- c.downN(2).as(morphir.ir._type.Codec.decodeType(decodeA))
            }  yield morphir.ir.Type.TypeAliasDefinition(
              arg1,
              arg2
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
              arg1 <- c.downN(1).as(morphir.sdk.list.Codec.decodeList(morphir.ir.name.Codec.decodeName))
              arg2 <- c.downN(2).as(morphir.ir._type.Codec.decodeConstructors(decodeA))
            }  yield morphir.ir.Type.CustomTypeSpecification(
              arg1,
              arg2
            )
          case """DerivedTypeSpecification""" => 
            for {
              arg1 <- c.downN(1).as(morphir.sdk.list.Codec.decodeList(morphir.ir.name.Codec.decodeName))
              arg2 <- c.downN(2).as(morphir.ir._type.Codec.decodeDerivedTypeSpecificationDetails(decodeA))
            }  yield morphir.ir.Type.DerivedTypeSpecification(
              arg1,
              arg2
            )
          case """OpaqueTypeSpecification""" => 
            for {
              arg1 <- c.downN(1).as(morphir.sdk.list.Codec.decodeList(morphir.ir.name.Codec.decodeName))
            }  yield morphir.ir.Type.OpaqueTypeSpecification(arg1)
          case """TypeAliasSpecification""" => 
            for {
              arg1 <- c.downN(1).as(morphir.sdk.list.Codec.decodeList(morphir.ir.name.Codec.decodeName))
              arg2 <- c.downN(2).as(morphir.ir._type.Codec.decodeType(decodeA))
            }  yield morphir.ir.Type.TypeAliasSpecification(
              arg1,
              arg2
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
              arg1 <- c.downN(1).as(decodeA)
              arg2 <- c.downN(2).as(morphir.ir.name.Codec.decodeName)
              arg3 <- c.downN(3).as(morphir.sdk.list.Codec.decodeList(morphir.ir._type.Codec.decodeField(decodeA)))
            }  yield morphir.ir.Type.ExtensibleRecord(
              arg1,
              arg2,
              arg3
            )
          case """Function""" => 
            for {
              arg1 <- c.downN(1).as(decodeA)
              arg2 <- c.downN(2).as(morphir.ir._type.Codec.decodeType(decodeA))
              arg3 <- c.downN(3).as(morphir.ir._type.Codec.decodeType(decodeA))
            }  yield morphir.ir.Type.Function(
              arg1,
              arg2,
              arg3
            )
          case """Record""" => 
            for {
              arg1 <- c.downN(1).as(decodeA)
              arg2 <- c.downN(2).as(morphir.sdk.list.Codec.decodeList(morphir.ir._type.Codec.decodeField(decodeA)))
            }  yield morphir.ir.Type.Record(
              arg1,
              arg2
            )
          case """Reference""" => 
            for {
              arg1 <- c.downN(1).as(decodeA)
              arg2 <- c.downN(2).as(morphir.ir.fqname.Codec.decodeFQName)
              arg3 <- c.downN(3).as(morphir.sdk.list.Codec.decodeList(morphir.ir._type.Codec.decodeType(decodeA)))
            }  yield morphir.ir.Type.Reference(
              arg1,
              arg2,
              arg3
            )
          case """Tuple""" => 
            for {
              arg1 <- c.downN(1).as(decodeA)
              arg2 <- c.downN(2).as(morphir.sdk.list.Codec.decodeList(morphir.ir._type.Codec.decodeType(decodeA)))
            }  yield morphir.ir.Type.Tuple(
              arg1,
              arg2
            )
          case """Unit""" => 
            for {
              arg1 <- c.downN(1).as(decodeA)
            }  yield morphir.ir.Type.Unit(arg1)
          case """Variable""" => 
            for {
              arg1 <- c.downN(1).as(decodeA)
              arg2 <- c.downN(2).as(morphir.ir.name.Codec.decodeName)
            }  yield morphir.ir.Type.Variable(
              arg1,
              arg2
            )
        })))

}