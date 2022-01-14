package zio.morphir

import zio.morphir.sexpr.ast.SExpr

package object sexpr {

  implicit class EncoderOps[A](val a: A) extends AnyVal {
    def toSExpr(implicit encoder: SExprEncoder[A]): String =
      encoder.encodeSExpr(a, None).toString

    def toSExprPretty(implicit encoder: SExprEncoder[A]): String =
      encoder.encodeSExpr(a, Some(0)).toString

    def toSExprPretty(indent: Int)(implicit encoder: SExprEncoder[A]): String =
      encoder.encodeSExpr(a, Some(indent)).toString

    def toSExprAST(implicit encoder: SExprEncoder[A]): Either[String, SExpr] = encoder.toAST(a)
  }

  implicit class DecoderOps(val sexpr: CharSequence) extends AnyVal {
    def fromSExpr[A](implicit A: SExprDecoder[A]): Either[String, A] = A.decodeSExpr(sexpr)
  }

  implicit class SExprOps[A](val sexpr: SExpr) extends AnyVal {
    def as[A](implicit A: SExprDecoder[A]): Either[String, A] = A.fromAST(sexpr)
  }

}
