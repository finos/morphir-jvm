package morphir.ir.rewriter

import zio._

import morphir.ir.PkgDef

object Rewriter {
  trait Service {
    def rewrite[A, B](f: PkgDef[A] => PkgDef[B], packageDef: PkgDef[A]): PkgDef[B]
  }

  val live: Layer[Nothing, Has[Service]] = ZLayer.succeed(new Service {
    def rewrite[A, B](f: PkgDef[A] => PkgDef[B], packageDef: PkgDef[A]): PkgDef[B] =
      f(packageDef)
  })
}
