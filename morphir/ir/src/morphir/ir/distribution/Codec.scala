package morphir.ir.distribution

/** Generated based on IR.Distribution
*/
object Codec{

  implicit val encodeComponent: io.circe.Encoder[morphir.ir.Distribution.Component] = ((component: morphir.ir.Distribution.Component) =>
    io.circe.Json.obj(
      ("""name""", morphir.ir.path.Codec.encodePath(component.name)),
      ("""libraries""", morphir.sdk.dict.Codec.encodeDict(
        morphir.ir._package.Codec.encodePackageName,
        morphir.ir._package.Codec.encodeDefinition(
          morphir.sdk.basics.Codec.encodeUnit,
          morphir.ir._type.Codec.encodeType(morphir.sdk.basics.Codec.encodeUnit)
        )
      )(component.libraries)),
      ("""inputs""", morphir.sdk.dict.Codec.encodeDict(
        morphir.ir.name.Codec.encodeName,
        morphir.ir._type.Codec.encodeType(morphir.sdk.basics.Codec.encodeUnit)
      )(component.inputs)),
      ("""states""", morphir.sdk.dict.Codec.encodeDict(
        morphir.ir.name.Codec.encodeName,
        morphir.ir._type.Codec.encodeType(morphir.sdk.basics.Codec.encodeUnit)
      )(component.states)),
      ("""outputs""", morphir.sdk.dict.Codec.encodeDict(
        morphir.ir.name.Codec.encodeName,
        morphir.ir.value.Codec.encodeValue(
          morphir.sdk.basics.Codec.encodeUnit,
          morphir.ir._type.Codec.encodeType(morphir.sdk.basics.Codec.encodeUnit)
        )
      )(component.outputs))
    ))
  
  implicit val encodeDistribution: io.circe.Encoder[morphir.ir.Distribution.Distribution] = ((distribution: morphir.ir.Distribution.Distribution) =>
    distribution match {
      case morphir.ir.Distribution.Library(packageName, dependencies, packageDef) => 
        io.circe.Json.arr(
          io.circe.Json.fromString("""Library"""),
          morphir.ir._package.Codec.encodePackageName(packageName),
          morphir.sdk.dict.Codec.encodeDict(
            morphir.ir._package.Codec.encodePackageName,
            morphir.ir._package.Codec.encodeSpecification(morphir.sdk.basics.Codec.encodeUnit)
          )(dependencies),
          morphir.ir._package.Codec.encodeDefinition(
            morphir.sdk.basics.Codec.encodeUnit,
            morphir.ir._type.Codec.encodeType(morphir.sdk.basics.Codec.encodeUnit)
          )(packageDef)
        )
    })
  
  implicit val decodeComponent: io.circe.Decoder[morphir.ir.Distribution.Component] = ((c: io.circe.HCursor) =>
    for {
      name_ <- c.downField("""name""").as(morphir.ir.path.Codec.decodePath)
      libraries_ <- c.downField("""libraries""").as(morphir.sdk.dict.Codec.decodeDict(
        morphir.ir._package.Codec.decodePackageName,
        morphir.ir._package.Codec.decodeDefinition(
          morphir.sdk.basics.Codec.decodeUnit,
          morphir.ir._type.Codec.decodeType(morphir.sdk.basics.Codec.decodeUnit)
        )
      ))
      inputs_ <- c.downField("""inputs""").as(morphir.sdk.dict.Codec.decodeDict(
        morphir.ir.name.Codec.decodeName,
        morphir.ir._type.Codec.decodeType(morphir.sdk.basics.Codec.decodeUnit)
      ))
      states_ <- c.downField("""states""").as(morphir.sdk.dict.Codec.decodeDict(
        morphir.ir.name.Codec.decodeName,
        morphir.ir._type.Codec.decodeType(morphir.sdk.basics.Codec.decodeUnit)
      ))
      outputs_ <- c.downField("""outputs""").as(morphir.sdk.dict.Codec.decodeDict(
        morphir.ir.name.Codec.decodeName,
        morphir.ir.value.Codec.decodeValue(
          morphir.sdk.basics.Codec.decodeUnit,
          morphir.ir._type.Codec.decodeType(morphir.sdk.basics.Codec.decodeUnit)
        )
      ))
    }  yield morphir.ir.Distribution.Component(
      name_,
      libraries_,
      inputs_,
      states_,
      outputs_
    ))
  
  implicit val decodeDistribution: io.circe.Decoder[morphir.ir.Distribution.Distribution] = ((c: io.circe.HCursor) =>
    c.withFocus(_.withString(((str) =>
      io.circe.Json.arr(io.circe.Json.fromString(str))))).downN(0).as(morphir.sdk.string.Codec.decodeString).flatMap(((tag) =>
      tag match {
        case """Library""" => 
          for {
            packageName <- c.downN(1).as(morphir.ir._package.Codec.decodePackageName)
            dependencies <- c.downN(2).as(morphir.sdk.dict.Codec.decodeDict(
              morphir.ir._package.Codec.decodePackageName,
              morphir.ir._package.Codec.decodeSpecification(morphir.sdk.basics.Codec.decodeUnit)
            ))
            packageDef <- c.downN(3).as(morphir.ir._package.Codec.decodeDefinition(
              morphir.sdk.basics.Codec.decodeUnit,
              morphir.ir._type.Codec.decodeType(morphir.sdk.basics.Codec.decodeUnit)
            ))
          }  yield morphir.ir.Distribution.Library(
            packageName,
            dependencies,
            packageDef
          )
      })))

}