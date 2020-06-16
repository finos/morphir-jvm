package morphir.ir.codec

import morphir.ir.documented.Documented
import upickle.default._

object documentedCodecs {

  trait DocumentedCodec {

    implicit def documentedReadWriter[A: ReadWriter]: ReadWriter[Documented[A]] =
      readwriter[(String, A)].bimap[Documented[A]](
        instance => (instance.doc, instance.value), {
          case (docs, value) => Documented(docs, value)
        }
      )

  }
}
