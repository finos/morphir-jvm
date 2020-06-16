package morphir.ir.codec

import morphir.ir.name.Name
import morphir.ir.argument.Argument
import upickle.default._

object argumentCodecs {

  trait ArgumentCodec {
    implicit def readWriter[A: ReadWriter]: ReadWriter[Argument[A]] =
      readwriter[(Name, A)].bimap[Argument[A]](
        arg => (arg.name, arg.value), {
          case (name, value) => Argument(name, value)
        }
      )
  }
}
