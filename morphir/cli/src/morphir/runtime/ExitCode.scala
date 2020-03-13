package morphir.runtime

sealed abstract class ExitCode(val code: Int)

object ExitCode {
  case object Success extends ExitCode(0)
  case object Failure extends ExitCode(1)
  case class Custom(override val code: Int) extends ExitCode(code)
}
