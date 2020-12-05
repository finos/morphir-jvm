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

package morphir.ir.loader

import zio._
import zio.test._
import zio.test.Assertion._
import zio.test.TestAspect._
import scala.io.Source
import java.io.IOException
import scala.io.BufferedSource

object LoaderSpec extends DefaultRunnableSpec {
  def spec =
    suite("Loader Spec")(
      suite("Loading from JSON String")(
        testM("Loading a simple package") {
          for {
            json   <- getManagedResource("morphir/ir/hello.ir.json").map(bs => bs.mkString).useNow
            pkgDef <- loadPackage(json).provideCustomLayer(Loader.live)
            _      <- console.putStrLn(s"Package: $pkgDef")
          } yield assert(pkgDef.modules)(isNonEmpty)
        } @@ silent,
        testM("Loading a package with custom types and access restrictions") {
          for {
            json   <- getManagedResource("morphir/ir/accounting.ir.json").map(bs => bs.mkString).useNow
            pkgDef <- loadPackage(json).provideCustomLayer(Loader.live)
            _      <- console.putStrLn(s"Package: $pkgDef")
          } yield assert(pkgDef.modules)(isNonEmpty)
        } @@ debug
      )
    )

  def getManagedResource(resource: String): Managed[IOException, BufferedSource] =
    Managed.make(IO.effect(Source.fromResource(resource)).refineToOrDie[IOException])(bs =>
      ZIO.effect(bs.close()).orDie
    )
}
