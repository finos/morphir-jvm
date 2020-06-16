package morphir.lang.scala

import zio._
package object naming {
  type Naming = Has[Naming.Service]
}
