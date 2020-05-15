package org.morphir.cli

import zio.{ UIO, ZIO }

sealed abstract class ExitCode(val code: Int)

object ExitCode {
  case object Success                       extends ExitCode(0)
  case object Failure                       extends ExitCode(1)
  case class Custom(override val code: Int) extends ExitCode(code)

  def success: UIO[Success.type]     = ZIO.succeed(Success)
  def failure: UIO[Failure.type]     = ZIO.succeed(Failure)
  def custom(code: Int): UIO[Custom] = ZIO.succeed(Custom(code))
}
