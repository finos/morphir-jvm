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

package morphir.lang.scala
import morphir.ir.FQName
import morphir.ir.name.Name

import scala.meta.{ Term, Type => MType }

object Naming {

  implicit class NameOps(name: Name) {
    def toTypeName: scala.meta.Type.Name =
      scala.meta.Type.Name(name.toString)

  }

  implicit class FQNameOps(private val fqn: FQName) extends AnyVal {
    def toTypeName: scala.meta.Type.Name =
      scala.meta.Type.Name(fqn.toString)

    def toTypeRef: MType.Ref = {
      val name = MType.Name(fqn.localName.toTitleCase)

      val parts =
        fqn.packagePath.mapSegments(name => Term.Name(name.toLowerCase)) ++ fqn.modulePath
          .mapSegments(name => Term.Name(name.toLowerCase))

      parts.foldLeft[MType.Ref](name) {
        case (localName: MType.Name, qual: Term.Name) => MType.Select(qual, localName)
        case (MType.Select(ref, localName), qual)     => MType.Select(Term.Select(ref, qual), localName)
      }

    }
  }
}
