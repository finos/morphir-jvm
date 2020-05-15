package morphir.lang

import zio._

package object scala {
  type ScalaCodeGenerator = Has[ScalaCodeGenerator.Service]
}
