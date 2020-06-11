package morphir.ir.codec

import morphir.ir.AccessControlled
import morphir.ir.json.JsonDecode.DecodeError
import upickle.default._

object accessControlledCodecs {

  trait AccessControlledCodec {

    implicit def readWriter[A: ReadWriter]: ReadWriter[AccessControlled[A]] =
      readwriter[(String, A)].bimap[AccessControlled[A]](
        {
          case AccessControlled.Public(value)  => ("public", value)
          case AccessControlled.Private(value) => ("private", value)
        }, {
          case ("public", value)  => AccessControlled.Public(value)
          case ("private", value) => AccessControlled.Private(value)
          case (tag, _)           => throw DecodeError.unexpectedTag(tag, "public", "private")
        }
      )
  }
}
