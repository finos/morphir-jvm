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


package morphir.ir

import zio._
import upickle.default._

package object loader {
  type Loader = Has[Loader.Service]

  def loadPackage(
    source: String
  )(implicit packageDefReader: Reader[PackageDefinition]): ZIO[Loader, Throwable, PackageDefinition] =
    ZIO.accessM(_.get.loadPackage(source))
}
