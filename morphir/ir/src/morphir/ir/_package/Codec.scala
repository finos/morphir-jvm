package morphir.ir._package

import scala.language.reflectiveCalls

/**
 * Generated based on IR.Package
 */
object Codec {

  implicit def encodeDefinition[Ta, Va](
    encodeTa: io.circe.Encoder[Ta],
    encodeVa: io.circe.Encoder[Va]
  ): io.circe.Encoder[morphir.ir.Package.Definition[Ta, Va]] =
    (
      (definition: {
        def modules: morphir.sdk.Dict.Dict[morphir.ir.Module.ModuleName, morphir.ir.AccessControlled.AccessControlled[
          morphir.ir.Module.Definition[Ta, Va]
        ]]
      }) =>
        io.circe.Json.obj(
          (
            "modules",
            morphir.sdk.dict.Codec.encodeDict(
              morphir.ir.module.Codec.encodeModuleName,
              morphir.ir.accesscontrolled.Codec.encodeAccessControlled(
                morphir.ir.module.Codec.encodeDefinition(
                  encodeTa,
                  encodeVa
                )
              )
            )(definition.modules)
          )
        )
    )

  implicit val encodePackageName: io.circe.Encoder[morphir.ir.Package.PackageName] = morphir.ir.path.Codec.encodePath

  implicit def encodeSpecification[Ta](
    encodeTa: io.circe.Encoder[Ta]
  ): io.circe.Encoder[morphir.ir.Package.Specification[Ta]] =
    (
      (specification: {
        def modules: morphir.sdk.Dict.Dict[morphir.ir.Module.ModuleName, morphir.ir.Module.Specification[Ta]]
      }) =>
        io.circe.Json.obj(
          (
            "modules",
            morphir.sdk.dict.Codec.encodeDict(
              morphir.ir.module.Codec.encodeModuleName,
              morphir.ir.module.Codec.encodeSpecification(encodeTa)
            )(specification.modules)
          )
        )
    )

  implicit def decodeDefinition[Ta, Va](
    decodeTa: io.circe.Decoder[Ta],
    decodeVa: io.circe.Decoder[Va]
  ): io.circe.Decoder[morphir.ir.Package.Definition[Ta, Va]] =
    (
      (c: io.circe.HCursor) =>
        for {
          modules_ <- c.downField("modules")
                        .as(
                          morphir.sdk.dict.Codec.decodeDict(
                            morphir.ir.module.Codec.decodeModuleName,
                            morphir.ir.accesscontrolled.Codec.decodeAccessControlled(
                              morphir.ir.module.Codec.decodeDefinition(
                                decodeTa,
                                decodeVa
                              )
                            )
                          )
                        )
        } yield morphir.ir.Package.Definition(modules_)
    )

  implicit val decodePackageName: io.circe.Decoder[morphir.ir.Package.PackageName] = morphir.ir.path.Codec.decodePath

  implicit def decodeSpecification[Ta](
    decodeTa: io.circe.Decoder[Ta]
  ): io.circe.Decoder[morphir.ir.Package.Specification[Ta]] =
    (
      (c: io.circe.HCursor) =>
        for {
          modules_ <- c.downField("modules")
                        .as(
                          morphir.sdk.dict.Codec.decodeDict(
                            morphir.ir.module.Codec.decodeModuleName,
                            morphir.ir.module.Codec.decodeSpecification(decodeTa)
                          )
                        )
        } yield morphir.ir.Package.Specification(modules_)
    )

}
