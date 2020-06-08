package morphir.ir

import morphir.ir.codec.documentedCodecs

object documented {

  case class Documented[+A](doc: String, value: A) {

    @inline def toTuple: (String, A) = (doc, value)

    def map[B](f: A => B): Documented[B] =
      Documented(doc, f(value))
  }

  object Documented extends documentedCodecs.DocumentedCodec {
    def fromTuple[A](tuple: (String, A)): Documented[A] = Documented(tuple._1, tuple._2)
  }
}
