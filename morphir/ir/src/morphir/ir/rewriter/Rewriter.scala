/*
Copyright 2020 Morgan Stanley

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/


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
