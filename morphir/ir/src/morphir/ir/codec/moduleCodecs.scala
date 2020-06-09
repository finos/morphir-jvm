package morphir.ir.codec

import io.circe.{ Decoder, Encoder, Json }
import io.circe.syntax._
import morphir.ir.documented.Documented
import morphir.ir.name.Name
import morphir.ir.{ AccessControlled, Type, Value }
import morphir.ir.module._
import upickle.default._
import morphir.ir.path.Path

object moduleCodecs {
  trait ModulePathCodec {

    implicit val modulePathReadWriter: ReadWriter[ModulePath] = readwriter[Path].bimap[ModulePath](
      mp => mp.toPath,
      p => ModulePath(p)
    )

    implicit val encodeModulePath: Encoder[ModulePath] =
      Path.encodePath.contramap(mp => mp.toPath)

    implicit val decodeModulePath: Decoder[ModulePath] =
      Path.decodePath.map(p => ModulePath(p))
  }

  object ModulePathCodec extends ModulePathCodec

  trait SpecificationCodec {
    implicit def moduleSpecReadWriter[A: ReadWriter]: ReadWriter[Specification[A]] =
      readwriter[ujson.Value].bimap[Specification[A]](
        _ => ???,
        _ => ???
      )

    implicit def encodeSpecification[A: Encoder]: Encoder[Specification[A]] = {
      implicit def encodeTypeSpecMap: Encoder[Map[Name, Documented[Type.Specification[A]]]] =
        Encoder.encodeList[(Name, Documented[Type.Specification[A]])].contramap(specMap => specMap.toList)

      implicit def encodeValueSpecMap: Encoder[Map[Name, Value.Specification[A]]] =
        Encoder.encodeList[(Name, Value.Specification[A])].contramap(_.toList)

      Encoder.encodeJson.contramap { spec =>
        Json.obj(
          ("type", spec.types.asJson),
          ("values", spec.values.asJson)
        )
      }
    }

    implicit def decodeSpecification[A: Decoder]: Decoder[Specification[A]] = ???
  }

  trait DefinitionCodec {
    implicit def moduleDefinitionReadWriter[A: ReadWriter]: ReadWriter[Definition[A]] =
      readwriter[ujson.Value].bimap[Definition[A]](
        definition =>
          ujson.Obj(
            ("types", writeJs(definition.types.toList)),
            ("values", ujson.Null)
          ),
        json => {
          val typesJson  = json("types")
          val valuesJson = json("values")
          val types = typesJson.arr.map { json =>
            val name = read[Name](json(0))
            val ac   = read[AccessControlled[Documented[Type.Definition[A]]]](json(1))
            (name, ac)
          }.toMap
          assert(typesJson != ujson.Null)
          assert(valuesJson != ujson.Null)
          Definition(types = types, values = Map.empty)
        }
      )
  }
}
