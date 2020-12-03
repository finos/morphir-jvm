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

package morphir.ir.codec

import morphir.ir.AccessControlled
import morphir.ir.json.JsonDecode.DecodeError
import upickle.default._

// scalafix:off DisableSyntax.throw
object accessControlledCodecs {

  trait AccessControlledCodec {

    implicit def readWriter[A: ReadWriter]: ReadWriter[AccessControlled[A]] =
      readwriter[(String, A)].bimap[AccessControlled[A]](
        {
          case AccessControlled.Public(value)  => ("public", value)
          case AccessControlled.Private(value) => ("private", value)
        },
        {
          case ("public", value)  => AccessControlled.publicAccess(value)
          case ("private", value) => AccessControlled.privateAccess(value)
          case (tag, _)           => throw DecodeError.unexpectedTag(tag, "public", "private")
        }
      )
  }
}
// scalafix:on
