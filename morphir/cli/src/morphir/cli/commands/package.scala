package morphir.cli

package object commands {

  import morphir.runtime.MorphirEnv
  import zio.ZIO

  type Cmdlet = ZIO[MorphirEnv, Nothing, ExitCode]
}
