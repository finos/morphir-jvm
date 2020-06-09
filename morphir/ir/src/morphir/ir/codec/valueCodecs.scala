package morphir.ir.codec

import morphir.ir.Value
import morphir.ir.json.Decode.DecodeError
import upickle.default._

object valueCodecs {
  trait ValueCodec {
    implicit def readWriter[A: ReadWriter]: ReadWriter[Value[A]] = readwriter[ujson.Value].bimap[Value[A]](
      valueExpr => ujson.Arr(ujson.Str(valueExpr.tag)),
      json => {
        val exprTag = json(0).str
        exprTag match {
          case tag => throw DecodeError.unexpectedTag(tag, List.empty)
        }
      }
    )
  }

  trait DefinitionCodec {
    implicit def readWriter[A: ReadWriter]: ReadWriter[Value.Definition[A]] =
      readwriter[ujson.Value].bimap[Value.Definition[A]](
        _ => ???,
        _ => ???
      )
  }
}
