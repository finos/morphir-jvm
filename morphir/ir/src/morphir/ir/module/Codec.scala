package morphir.ir.module

/** Generated based on IR.Module
*/
object Codec{

  implicit def encodeDefinition[Ta, Va](
    encodeTa: io.circe.Encoder[Ta],
    encodeVa: io.circe.Encoder[Va]
  ): io.circe.Encoder[morphir.ir.Module.Definition[Ta, Va]] =
    ((definition: morphir.ir.Module.Definition[Ta, Va]) =>
      io.circe.Json.obj(
        ("types", morphir.sdk.dict.Codec.encodeDict(
          morphir.ir.name.Codec.encodeName,
          morphir.ir.accesscontrolled.Codec.encodeAccessControlled(morphir.ir.documented.Codec.encodeDocumented(morphir.ir._type.Codec.encodeDefinition(encodeTa)))
        )(definition.types)),
        ("values", morphir.sdk.dict.Codec.encodeDict(
          morphir.ir.name.Codec.encodeName,
          morphir.ir.accesscontrolled.Codec.encodeAccessControlled(morphir.ir.documented.Codec.encodeDocumented(morphir.ir.value.Codec.encodeDefinition(
            encodeTa,
            encodeVa
          )))
        )(definition.values)),
        ("doc", morphir.sdk.maybe.Codec.encodeMaybe(morphir.sdk.string.Codec.encodeString)(definition.doc))
      ))
  
  implicit val encodeModuleName: io.circe.Encoder[morphir.ir.Module.ModuleName] = morphir.ir.path.Codec.encodePath
  
  implicit val encodeQualifiedModuleName: io.circe.Encoder[morphir.ir.Module.QualifiedModuleName] = ((qualifiedModuleName: (morphir.ir.Path.Path, morphir.ir.Path.Path)) =>
    io.circe.Json.arr(
      morphir.ir.path.Codec.encodePath(qualifiedModuleName._1),
      morphir.ir.path.Codec.encodePath(qualifiedModuleName._2)
    ))
  
  implicit def encodeSpecification[Ta](
    encodeTa: io.circe.Encoder[Ta]
  ): io.circe.Encoder[morphir.ir.Module.Specification[Ta]] =
    ((specification: morphir.ir.Module.Specification[Ta]) =>
      io.circe.Json.obj(
        ("types", morphir.sdk.dict.Codec.encodeDict(
          morphir.ir.name.Codec.encodeName,
          morphir.ir.documented.Codec.encodeDocumented(morphir.ir._type.Codec.encodeSpecification(encodeTa))
        )(specification.types)),
        ("values", morphir.sdk.dict.Codec.encodeDict(
          morphir.ir.name.Codec.encodeName,
          morphir.ir.documented.Codec.encodeDocumented(morphir.ir.value.Codec.encodeSpecification(encodeTa))
        )(specification.values)),
        ("doc", morphir.sdk.maybe.Codec.encodeMaybe(morphir.sdk.string.Codec.encodeString)(specification.doc))
      ))
  
  implicit def decodeDefinition[Ta, Va](
    decodeTa: io.circe.Decoder[Ta],
    decodeVa: io.circe.Decoder[Va]
  ): io.circe.Decoder[morphir.ir.Module.Definition[Ta, Va]] =
    ((c: io.circe.HCursor) =>
      for {
        types_ <- c.downField("types").as(morphir.sdk.dict.Codec.decodeDict(
          morphir.ir.name.Codec.decodeName,
          morphir.ir.accesscontrolled.Codec.decodeAccessControlled(morphir.ir.documented.Codec.decodeDocumented(morphir.ir._type.Codec.decodeDefinition(decodeTa)))
        ))
        values_ <- c.downField("values").as(morphir.sdk.dict.Codec.decodeDict(
          morphir.ir.name.Codec.decodeName,
          morphir.ir.accesscontrolled.Codec.decodeAccessControlled(morphir.ir.documented.Codec.decodeDocumented(morphir.ir.value.Codec.decodeDefinition(
            decodeTa,
            decodeVa
          )))
        ))
        doc_ <- c.downField("doc").as(morphir.sdk.maybe.Codec.decodeMaybe(morphir.sdk.string.Codec.decodeString))
      }  yield morphir.ir.Module.Definition(
        types_,
        values_,
        doc_
      ))
  
  implicit val decodeModuleName: io.circe.Decoder[morphir.ir.Module.ModuleName] = morphir.ir.path.Codec.decodePath
  
  implicit val decodeQualifiedModuleName: io.circe.Decoder[morphir.ir.Module.QualifiedModuleName] = ((c: io.circe.HCursor) =>
    for {
      arg1 <- c.downN(0).as(morphir.ir.path.Codec.decodePath)
      arg2 <- c.downN(1).as(morphir.ir.path.Codec.decodePath)
    }  yield (arg1, arg2))
  
  implicit def decodeSpecification[Ta](
    decodeTa: io.circe.Decoder[Ta]
  ): io.circe.Decoder[morphir.ir.Module.Specification[Ta]] =
    ((c: io.circe.HCursor) =>
      for {
        types_ <- c.downField("types").as(morphir.sdk.dict.Codec.decodeDict(
          morphir.ir.name.Codec.decodeName,
          morphir.ir.documented.Codec.decodeDocumented(morphir.ir._type.Codec.decodeSpecification(decodeTa))
        ))
        values_ <- c.downField("values").as(morphir.sdk.dict.Codec.decodeDict(
          morphir.ir.name.Codec.decodeName,
          morphir.ir.documented.Codec.decodeDocumented(morphir.ir.value.Codec.decodeSpecification(decodeTa))
        ))
        doc_ <- c.downField("doc").as(morphir.sdk.maybe.Codec.decodeMaybe(morphir.sdk.string.Codec.decodeString))
      }  yield morphir.ir.Module.Specification(
        types_,
        values_,
        doc_
      ))

}