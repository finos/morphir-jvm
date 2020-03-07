package morphir
import zio._

package object runtime {
  type MorphirEnv = ZEnv
  type Cmdlet = ZIO[MorphirEnv, Nothing, ExitCode]
}
