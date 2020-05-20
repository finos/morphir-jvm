package morphir.lang

import zio._

package object scala {
  type ScalaBackend = Has[ScalaBackend.Service]
}
